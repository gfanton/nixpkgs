{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.colima;
  homeDir = config.users.users.${config.users.primaryUser.username}.home;
in
{
  options = {
    services.colima = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Colima container runtime daemon.";
      };

      package = mkOption {
        type = types.package;
        default = pkgs.colima;
        description = "The Colima package to use.";
      };

      profile = mkOption {
        type = types.str;
        default = "default";
        description = "The Colima profile to use.";
      };

      autoStart = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to automatically start Colima on system boot.";
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.colima = mkIf cfg.autoStart {
      # docker-client lives only in the per-user profile, whose systemPath entry
      # is `/etc/profiles/per-user/$USER/bin` — launchd never expands $USER, so
      # colima's docker dependency check fails. Pin the absolute store path.
      path = [
        pkgs.docker-client
        config.environment.systemPath
      ];
      serviceConfig = {
        ProgramArguments = [
          "${cfg.package}/bin/colima"
          "start"
          "--profile"
          "${cfg.profile}"
          "--save-config=false"
        ];
        RunAtLoad = true;
        KeepAlive = {
          SuccessfulExit = false;
        };
        StandardErrorPath = "/tmp/colima.log";
        StandardOutPath = "/tmp/colima.log";
        # launchd agents inherit no shell environment and plist values are
        # literal strings, so COLIMA_HOME must be an absolute path. Without
        # it colima falls back to ~/.colima on macOS.
        EnvironmentVariables = {
          COLIMA_HOME = "${homeDir}/.config/colima";
        };
      };
    };
  };
}
