{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.colima;
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
        # Set environment variables for XDG compliance
        EnvironmentVariables = {
          COLIMA_CONFIG_DIR = "%HOME%/.config/colima";
          COLIMA_DATA_DIR = "%HOME%/.local/share/colima";
        };
      };
    };
  };
}
