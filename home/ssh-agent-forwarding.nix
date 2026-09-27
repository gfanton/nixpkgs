{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.home.user-info) sshAgent forwardAgentTo;

  # Where a forwarded agent lands on the receiving host. Each end derives it
  # from its own home directory, which is the same path because the user name
  # is the same on every Mac.
  forwardedSocket = "${config.home.homeDirectory}/.ssh/agent.sock";
  onePasswordSocket = "${config.home.homeDirectory}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";
in
lib.mkMerge [
  # mosh carries no agent, and each SSH connection forwards its own socket
  # that dies with it, so every shell here points at the one path the
  # standing connection keeps alive.
  (lib.mkIf (sshAgent == "forwarded") {
    home.sessionVariables.SSH_AUTH_SOCK = forwardedSocket;
  })

  # One connection per host, doing nothing but publishing the 1Password
  # agent at forwardedSocket there. launchd restarts it whenever it drops,
  # so the forward follows the laptop through sleep and network changes.
  (lib.mkIf (pkgs.stdenv.isDarwin && forwardAgentTo != [ ]) {
    programs.ssh.settings = lib.listToAttrs (
      map (host: {
        name = "${host}-agent";
        value = {
          HostName = host;
          RemoteForward = "${forwardedSocket} \"${onePasswordSocket}\"";
          ExitOnForwardFailure = true;
          # A dedicated connection: never a multiplexing master other
          # sessions would ride on and keep alive.
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
