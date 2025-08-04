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

      profile = mkOption {
        type = types.str;
        default = "nix-vanilla";
        description = "Emacs profile to use with chemacs2.";
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
        exec ${cfg.package}/bin/emacs --with-profile=${cfg.profile} --fg-daemon
      '';
    in
    mkIf cfg.enable {
      launchd.user.agents.my-emacs = {
        path = cfg.additionalPath ++ [ environmentSystemPath ];
        serviceConfig = {
          Label = "org.gnu.emacs.daemon";
          ProgramArguments = [
            "${pkgs.zsh}/bin/zsh"
            "${emacs-daemon}/bin/emacs-daemon"
          ];
          RunAtLoad = true;
          KeepAlive = true;
          StandardErrorPath = logFile;
          StandardOutPath = logFile;
        };
      };

      # Create log directory
      system.activationScripts.my-emacs.text = ''
        echo "Creating Emacs daemon log directory..."
        mkdir -p ${logDir}
        chown ${primaryUser}:staff ${logDir}
      '';
    };
}
