{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.my-emacs;

  # Use proper Nix system variables
  primaryUser = config.users.primaryUser.username;
  homeDir = config.users.users.${primaryUser}.home;
  systemPath = config.system.path;
  environmentSystemPath = config.environment.systemPath;

  # Profile paths using standard Nix patterns from environment.profiles
  userNixProfile = "${homeDir}/.nix-profile";
  perUserProfile = "/etc/profiles/per-user/${primaryUser}";
  nixStateProfile = "${homeDir}/.local/state/nix/profile";
in

{
  options = {
    services.my-emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the Emacs Daemon.";
      };

      package = mkOption {
        type = types.package;
        default = pkgs.emacs;
        description = "The Emacs package to use.";
      };

      additionalPath = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Extra PATH entries for the daemon.";
      };
    };
  };

  config =
    let
      # Log paths using XDG pattern
      logDir = "${homeDir}/.local/state/emacs";
      logFile = "${logDir}/daemon.log";

      label = "org.gnu.emacs.daemon";

      # Terminfo directories using standard Nix profile patterns and system variables
      terminfoPath = concatStringsSep ":" [
        "${homeDir}/.terminfo" # User custom terminfo
        "${perUserProfile}/share/terminfo" # Per-user profile terminfo
        "${userNixProfile}/share/terminfo" # User nix profile terminfo
        "${nixStateProfile}/share/terminfo" # XDG state profile terminfo
        "${systemPath}/share/terminfo" # System path terminfo
        "/usr/share/terminfo" # System default terminfo
      ];

      emacs-daemon = pkgs.writeShellScriptBin "emacs-daemon" ''
        # Set up proper environment for the daemon
        export TERM=xterm-emacs
        export COLORTERM=truecolor
        export TERMINFO_DIRS="${terminfoPath}"
        export LC_ALL=en_US.UTF-8
        export LANG=en_US.UTF-8

        # Launch Emacs daemon
        exec ${cfg.package}/bin/emacs --fg-daemon
      '';
    in
    mkIf cfg.enable {
      launchd.user.agents.my-emacs = {
        path = cfg.additionalPath ++ [ environmentSystemPath ];
        serviceConfig = {
          Label = label;
          ProgramArguments = [
            "${pkgs.zsh}/bin/zsh"
            "${emacs-daemon}/bin/emacs-daemon"
          ];
          RunAtLoad = true;
          KeepAlive = true;
          StandardErrorPath = logFile;
          StandardOutPath = logFile;
          # launchd's default soft limit is 256 FDs; kqueue file-notify costs
          # one FD per watched directory, so magit/lsp watchers exhaust it.
          SoftResourceLimits.NumberOfFiles = 4096;
        };
      };

      # postActivation runs after userLaunchd, so the agent has been (re)loaded
      # by the time this runs. nix-darwin only reloads the agent when the plist
      # changes, and its legacy `launchctl load -w` unreliably honors RunAtLoad
      # in the GUI session — which can leave the daemon dead after a rebuild
      # that touched it. `kickstart` (no -k) guarantees the daemon is running
      # without restarting a healthy one, so open buffers survive unrelated
      # rebuilds. A custom activationScripts.<name> key would be silently
      # dropped: only the predefined phases are run.
      system.activationScripts.postActivation.text = ''
        echo "Ensuring Emacs daemon log directory and service..." >&2
        mkdir -p ${logDir}
        chown ${primaryUser}:staff ${logDir}
        uid=$(id -u ${primaryUser})
        launchctl kickstart "gui/$uid/${label}" || true
      '';
    };
}
