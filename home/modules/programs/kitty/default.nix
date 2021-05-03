{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.programs.mykitty;

  eitherStrBoolInt = with types; either str (either bool int);

  optionalPackage = opt:
    optional (opt != null && opt.package != null) opt.package;

  toKittyConfig = generators.toKeyValue {
    mkKeyValue = key: value:
      let
        value' = if isBool value then
          (if value then "yes" else "no")
        else
          toString value;
      in "${key} ${value'}";
  };

  toKittyKeybindings = generators.toKeyValue {
    mkKeyValue = key: command: "map ${key} ${command}";
  };

in {
  options.programs.mykitty = {
    enable = mkEnableOption "Kitty terminal emulator";

    package = mkOption {
      type = types.package;
      default = pkgs.kitty;
      defaultText = literalExample "pkgs.kitty";
      example = literalExample "pkgs.kitty";
      description = "The Kitty package to use.";
    };

    settings = mkOption {
      type = types.attrsOf eitherStrBoolInt;
      default = { };
      example = literalExample ''
        {
          scrollback_lines = 10000;
          enable_audio_bell = false;
          update_check_interval = 0;
        }
      '';
      description = ''
        Configuration written to
        <filename>~/.config/kitty/kitty.conf</filename>. See
        <link xlink:href="https://sw.kovidgoyal.net/kitty/conf.html" />
        for the documentation.
      '';
    };

    font = mkOption {
      type = types.nullOr hm.types.fontType;
      default = null;
      description = "The font to use.";
    };

    keybindings = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Mapping of keybindings to actions.";
      example = literalExample ''
        {
          "ctrl+c" = "copy_or_interrupt";
          "ctrl+f>2" = "set_font_size 20";
        }
      '';
    };

    extraConfig = mkOption {
      default = "";
      type = types.lines;
      description = "Additional configuration to add.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.bc cfg.package ] ++ optionalPackage cfg.font;

    xdg.configFile."kitty/kitty.conf".text = ''
      # Generated by Home Manager.
      # See https://sw.kovidgoyal.net/kitty/conf.html
      ${optionalString (cfg.font != null) ''
        font_family ${cfg.font.name}
        ${optionalString (cfg.font.size != null)
        "font_size ${toString cfg.font.size}"}
      ''}
      ${toKittyConfig cfg.settings}
      ${toKittyKeybindings cfg.keybindings}
      ${cfg.extraConfig}
    '';
  };
}