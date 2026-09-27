{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.home.user-info) sshAgent forwardAgentTo;

  # The same path on both ends: every Mac has the same user name.
  forwardedSocket = "${config.home.homeDirectory}/.ssh/agent.sock";
  onePasswordSocket = "${config.home.homeDirectory}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";
in
lib.mkMerge [
  (lib.mkIf (sshAgent == "forwarded") {
    home.sessionVariables.SSH_AUTH_SOCK = forwardedSocket;
  })

  (lib.mkIf (pkgs.stdenv.isDarwin && forwardAgentTo != [ ]) {
    programs.ssh.settings = lib.listToAttrs (
      map (host: {
        name = "${host}-agent";
        value = {
          HostName = host;
          RemoteForward = "${forwardedSocket} \"${onePasswordSocket}\"";
          ExitOnForwardFailure = true;
          ControlMaster = false;
          ControlPath = "none";
          ServerAliveInterval = 15;
          ServerAliveCountMax = 2;
        };
      }) forwardAgentTo
    );

    launchd.agents = lib.listToAttrs (
      map (host: {
        name = "ssh-agent-forward-${host}";
        value = {
          enable = true;
          config = {
            ProgramArguments = [
              "/usr/bin/ssh"
              "-N"
              "${host}-agent"
            ];
            KeepAlive = true;
            StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/ssh-agent-forward-${host}.log";
          };
        };
      }) forwardAgentTo
    );
  })
]
