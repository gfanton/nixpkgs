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

    # asdf-install = map (tool:
    #   pkgs.runCommandLocal "${tool.name}-${tool.version}" {
    #     name = "${tool.name}";
    #     buildInputs = with pkgs;
    #       [ cfg.package curl gitAndTools.git ]
    #       ++ lib.optionals stdenv.isDarwin [
    #         darwin.DarwinTools
    #         darwin.bootstrap_cmds
    #       ] ++ tool.inputs;
    #   } ''

    #     set -x

    #     export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
    #     export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

    #     export ASDF_DIR="${cfg.package}/share/asdf-vm"
    #     export ASDF_CONFIG_FILE=${asdf-config}
    #     export ASDF_DATA_DIR=$out/share/${tool.name}

    #     mkdir -p $ASDF_DATA_DIR/installs/${tool.name}
    #     touch $ASDF_DATA_DIR/installs/${tool.name}/.keep

    #     [ -z "${cfg.pluginsRepository}" ] || ln -s ${cfg.pluginsRepository} $ASDF_DATA_DIR/repository

    #     asdf plugin add ${tool.name}
    #     if [ ! -z "${tool.version}" ] && [ "${tool.version}" != "system" ]; then
    #        ${tool.preInstall}
    #        asdf install ${tool.name} ${tool.version}
    #        asdf reshim ${tool.name} ${tool.version}
    #        ${tool.postInstall}
    #     fi

    #     cd $ASDF_DATA_DIR && rm -rf tmp downloads
    #   '') cfg.tools;
  in mkIf config.programs.myasdf.enable {
    home.packages = [ cfg.package ];
    # xdg.configFile."asdf/asdfrc".source = asdf-config;

    # home.file = listToAttrs ((map (t: {
    #   name = "${cfg.asdfdir}/plugins/${t.name}";
    #   value = {
    #     source = "${t}/share/${t.name}/plugins/${t.name}";
    #     recursive = true;
    #   };
    # }) asdf-install) ++ (map (t: {
    #   name = "${cfg.asdfdir}/installs/${t.name}";
    #   value = {
    #     source = "${t}/share/${t.name}/installs/${t.name}";
    #     recursive = true;
    #   };
    # }) asdf-install) ++
    home.file = listToAttrs [
      # {
      #   name = ".tool-versions";
      #   value.source = asdf-tools;
      # }
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
      export PATH=${pkgs.gawk}/bin:${pkgs.gitAndTools.git}/bin:${pkgs.curl}/bin:$PATH
      export ASDF_CONFIG_FILE="${config.xdg.configHome}/asdf/asdfrc"
      export ASDF_DATA_DIR="${cfg.asdfdir}"
      export ASDF_DIR="${cfg.package}/share/asdf-vm"

      ${lib.concatStringsSep "\n" (map (t: ''
        ${cfg.package}/bin/asdf plugin add ${t.name} || true
        if [ ! -z "${t.version}" ] && [ "${t.version}" != "system" ]; then
           ${cfg.package}/bin/asdf global ${t.name} ${t.version} || true
           ${cfg.package}/bin/asdf reshim ${t.name} ${t.version} || true
        fi
      '') cfg.tools)}
    '';

    programs.zsh.initExtra = ''
      if [ -f "${cfg.package}/share/asdf-vm/asdf.sh" ]; then
        . "${cfg.package}/share/asdf-vm/asdf.sh"
      fi

      fpath=(${cfg.package}/share/asdf-vm/completions $fpath)
    '';
  };
}
