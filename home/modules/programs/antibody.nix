{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.zsh.antibody;
  pluginModule = types.submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        description = "The name of the plugin.";
      };

      tags = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "The plugin tags.";
      };
    };
  });
in
{
  options.programs.zsh.antibody = {
    enable = mkEnableOption "antibody - zsh plugins manager";
    package = mkOption {
      type = types.package;
      default = pkgs.antibody;
      defaultText = literalExample "pkgs.antibody";
      example = literalExample "pkgs.antibody";
      description = "The antibody package to use.";
    };
    plugins = mkOption {
      default = [ ];
      type = types.listOf types.str;
      description = "List of zsh_plugins.";
    };
    configPlugins = mkOption {
      type = types.lines;
      description = "plugins init";
    };
  };

  config =
    let
      plugins = pkgs.writeText "plugins" ''
        # auto generated files
        ${lib.concatStringsSep "\n" cfg.plugins}
      '';
      antibody-gen = pkgs.runCommandLocal "antibody-gen" {
        buildInputs = [ pkgs.gitAndTools.git cfg.package ];
      } ''
          export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
          export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
          export ANTIBODY_HOME=$out/share/antibody
          mkdir -p $out/share/antibody
          ${cfg.package}/bin/antibody bundle < ${plugins} > $out/share/antibody/zsh-plugins.sh
        '';
    in mkIf config.programs.zsh.antibody.enable {
        home.packages = [ antibody-gen ];
        programs.zsh.initExtra = ''
          if [ -f "${antibody-gen}/share/antibody/zsh-plugins.sh" ]; then
            source "${antibody-gen}/share/antibody/zsh-plugins.sh"
          fi

          ${cfg.configPlugins}
        '';
      };
}
