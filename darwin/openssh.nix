{
  config,
  lib,
  pkgs,
  ...
}:

let
  routedPrivate = config.services.openssh.routedPrivateAccess;
in
{
  options.services.openssh.routedPrivateAccess = {
    enable = lib.mkEnableOption "sshd and mosh for routed private networks, with the gateway's firewall choosing which sources reach them";
    interface = lib.mkOption {
      type = lib.types.str;
      example = "en0";
      description = "LAN interface whose own subnet stays refused: hosts on it never cross the gateway's firewall.";
    };
  };

  config = {
    # ---- SSH server
    # Enabled via launchctl bootstrap of /System/Library/LaunchDaemons/ssh.plist.
    # sshd is socket-activated by launchd and ignores ListenAddress / Port from
    # sshd_config — restriction must be done with pf below.
    services.openssh = {
      enable = true;
      extraConfig = ''
        PasswordAuthentication no
        PermitRootLogin no
        KbdInteractiveAuthentication no
        AuthenticationMethods publickey
      ''
      + lib.optionalString (config.users.primaryUser.sshAgent == "forwarded") ''
        StreamLocalBindUnlink yes
      '';
    };

    # ---- pf rules: sshd (22) and mosh (60000-61000) accept Tailscale, plus routed
    # private networks where routedPrivateAccess is on. Screen Sharing (5900) accepts
    # Tailscale only.
    # Tailscale IPv4 CGNAT: 100.64.0.0/10. Tailscale IPv6 ULA: fd7a:115c:a1e0::/48.
    environment.etc."pf.user.conf".text = ''
      # Preserve macOS default anchors (other system services keep working).
      scrub-anchor "com.apple/*"
      nat-anchor "com.apple/*"
      rdr-anchor "com.apple/*"
      dummynet-anchor "com.apple/*"
      anchor "com.apple/*"
      load anchor "com.apple" from "/etc/pf.anchors/com.apple"

      # Loopback always passes.
      pass in quick on lo0 all
      pass out quick on lo0 all

      # Tailscale IPv4 (CGNAT).
      pass in quick proto tcp from 100.64.0.0/10 to any port 22
      pass in quick proto tcp from 100.64.0.0/10 to any port 5900
      pass in quick proto udp from 100.64.0.0/10 to any port 60000:61000

      # Tailscale IPv6 (ULA).
      pass in quick inet6 proto tcp from fd7a:115c:a1e0::/48 to any port 22
      pass in quick inet6 proto tcp from fd7a:115c:a1e0::/48 to any port 5900
      pass in quick inet6 proto udp from fd7a:115c:a1e0::/48 to any port 60000:61000
    ''
    + lib.optionalString routedPrivate.enable ''

      # Same-subnet hosts never cross the gateway's firewall, so refuse them here.
      block return in quick proto tcp from (${routedPrivate.interface}:network) to any port { 22 5900 }
      block return in quick proto udp from (${routedPrivate.interface}:network) to any port 60000:61000

      # Routed private networks: the gateway's firewall decides which reach these ports.
      pass in quick proto tcp from { 10.0.0.0/8 172.16.0.0/12 192.168.0.0/16 } to any port 22
      pass in quick proto udp from { 10.0.0.0/8 172.16.0.0/12 192.168.0.0/16 } to any port 60000:61000
    ''
    + ''

      # Drop everything else hitting these ports.
      block return in quick proto tcp to any port 22
      block return in quick proto tcp to any port 5900
      block return in quick proto udp to any port 60000:61000

      # Default policy: preserve normal connectivity for all other traffic.
      pass out all keep state
      pass in all
    '';

    # Load pf rules at boot.
    launchd.daemons.pf-tailscale-only = {
      serviceConfig = {
        Label = "user.pf.tailscale-only";
        ProgramArguments = [
          "/sbin/pfctl"
          "-E"
          "-f"
          "/etc/pf.user.conf"
        ];
        RunAtLoad = true;
        KeepAlive = false;
        StandardOutPath = "/var/log/pf-tailscale-only.log";
        StandardErrorPath = "/var/log/pf-tailscale-only.log";
      };
    };

    # Re-apply pf rules on darwin-rebuild switch so changes take effect without reboot.
    # Nix builds are ad-hoc signed, so allowSigned does not cover mosh-server;
    # the allowance follows its signing identity, not its store path.
    system.activationScripts.postActivation.text = lib.mkAfter ''
      /sbin/pfctl -E -f /etc/pf.user.conf || true

      socketfilterfw=/usr/libexec/ApplicationFirewall/socketfilterfw
      moshServer=${lib.getExe' pkgs.mosh "mosh-server"}
      if [[ $("$socketfilterfw" --getappblocked "$moshServer") != *"is permitted"* ]]; then
        "$socketfilterfw" --add "$moshServer" >/dev/null || true
        "$socketfilterfw" --unblockapp "$moshServer" >/dev/null || true
      fi
      if [[ $("$socketfilterfw" --getappblocked "$moshServer") != *"is permitted"* ]]; then
        echo "warning: the application firewall blocks $moshServer, so mosh cannot connect" >&2
      fi
    '';
  };
}
