{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.myasdf;
  toolModule = types.submodule ({ config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        description = "The name of the tool.";
      };

      version = mkOption {
        type = types.str;
        default = "";
        description = "tool version.";
      };

      inputs = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "list of build inputs";
      };

      preInstall = mkOption {
        type = types.str;
        default = "";
        description = "pre install command";
      };

      postInstall = mkOption {
        type = types.str;
        default = "";
        description = "post install command";
      };
    };
  });
in {
  options.programs.myasdf = {
    enable = mkEnableOption "asdf tools manager";

    package = mkOption {
      type = types.package;
      default = pkgs.asdf-vm;
      defaultText = literalExample "pkgs.asdf-vm";
      example = literalExample "pkgs.asdf-vm";
      description = "The asdf package to use.";
    };

    pluginsRepository = mkOption {
      type = types.package;
      default = "";
      description = "plugins repository";
    };

    tools = mkOption {
      type = types.listOf toolModule;
      default = [ ];
      description = "list of asdf tools";
    };

    asdfdir = mkOption {
      type = types.str;
      default = "${config.xdg.dataHome}/asdf";
    };
  };

  config = let
    asdf-activiation-inputs = lib.concatStringsSep ":" (map (t: "${t}/bin")
      [ pkgs.curl pkgs.gawk pkgs.gnutar pkgs.gitAndTools.git pkgs.gzip pkgs.zip pkgs.unzip ]);

    asdf-config = pkgs.writeText "asdfrc" ''
      # See the docs for explanations: https://asdf-vm.com/manage/configuration.html

      legacy_version_file = no
      use_release_candidates = no
      always_keep_download = no
      plugin_repository_last_check_duration = never
      disable_plugin_short_name_repository = no
    '';

    asdf-tools = pkgs.writeText "tool-versions" ''
      # auto-generated file, do not edit

      ${lib.concatStringsSep "\n" (map (tool: "${tool.name} ${tool.version}")
        (filter (tool: tool.version != "" && tool.version != "latest")
          cfg.tools))}
    '';

  in mkIf config.programs.myasdf.enable {
    home.packages = [ cfg.package ];

    home.file = listToAttrs [
      {
        name = "${cfg.asdfdir}/asdf_updates_disabled";
        value.text = "";
      }
      {
        name = "${cfg.asdfdir}/repository";
        value = {
          source = cfg.pluginsRepository;
          recursive = true;
        };
      }
    ];

    # # asdf env variables
    home.sessionVariables = {
      ASDF_CONFIG_FILE = "${config.xdg.configHome}/asdf/asdfrc";
      ASDF_DATA_DIR = "${cfg.asdfdir}";
      ASDF_DIR = "${cfg.package}/share/asdf-vm";
    };

    home.activation.reshimsASDF = hm.dag.entryAfter [ "installpackages" ] ''
      export PATH=${asdf-activiation-inputs}:$PATH
      export ASDF_CONFIG_FILE="${config.xdg.configHome}/asdf/asdfrc"
      export ASDF_DATA_DIR="${cfg.asdfdir}"
      export ASDF_DIR="${cfg.package}/share/asdf-vm"

      mkdir -p $ASDF_DATA_DIR

      ${cfg.package}/bin/asdf plugin update --all
      ${lib.concatStringsSep "\n" (map (t: ''
         if ! ${cfg.package}/bin/asdf plugin list | grep ${t.name} 1>/dev/null; then
            ${cfg.package}/bin/asdf plugin add ${t.name}
         fi

        if [ ! -z "${t.version}" ] && [ "${t.version}" != "system" ]; then
           ${cfg.package}/bin/asdf global ${t.name} ${t.version} || true
        fi
      '') cfg.tools)}

      ${cfg.package}/bin/asdf install
      ${cfg.package}/bin/asdf reshim
    '';


    programs.zsh.initExtra = ''
      if [ -f "${cfg.package}/share/asdf-vm/asdf.sh" ]; then
        . "${cfg.package}/share/asdf-vm/asdf.sh"
      fi

      fpath=(${cfg.package}/share/asdf-vm/completions $fpath)
    '';
  };
}
