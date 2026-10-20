{
  config,
  lib,
  pkgs,
  ...
}:

let
  sshKeys = import ../lib/ssh-keys.nix;
in
{
  services.tailscale.enable = true;
  launchd.daemons.tailscaled.serviceConfig.KeepAlive = true;

  services.openssh.routedPrivateAccess = {
    enable = true;
    interface = "en0";
  };

  users.primaryUser.sshAgent = "forwarded";
  users.primaryUser.sshKey = sshKeys.kalamata;

  users.users.${config.users.primaryUser.username}.openssh.authorizedKeys.keys = [ sshKeys.tzatziki ];

  power.restartAfterPowerFailure = true;
  power.restartAfterFreeze = true;

  # sudo asks aye's approvers on this Mac alone: these lines name aye's module, and on a Mac
  # without it a sudo_local line naming a missing module makes every password sudo fail.
  security.pam.services.sudo_local.text = lib.mkForce ''
    auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so
    auth       requisite      /usr/local/lib/pam/pam_aye.so role=gate
    auth       sufficient     /usr/local/lib/pam/pam_aye.so role=pass
  '';
}
