{ config, lib, ... }:

let
  inherit (config.home.user-info) sshKey forwardAgentTo;
in
lib.mkIf (sshKey != null) {
  home.file.".ssh/device.pub".text = sshKey;

  programs.ssh.settings =
    lib.genAttrs ([ "github.com" ] ++ forwardAgentTo ++ map (host: "${host}-agent") forwardAgentTo)
      (_: {
        IdentityFile = "~/.ssh/device.pub";
        IdentitiesOnly = true;
      });
}
