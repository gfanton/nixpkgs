{
  name,
  config,
  lib,
  ...
}:

let
  inherit (lib)
    attrNames
    attrValues
    hasPrefix
    listToAttrs
    literalExpression
    mapAttrs
    mkOption
    range
    types
    ;

  baseColorOptions =
    listToAttrs (
      map (i: {
        name = "color${toString i}";
        value = mkOption { type = types.str; };
      }) (range 0 15)
    )
    // listToAttrs (
      map (i: {
        name = "color${toString i}";
        value = mkOption {
          default = "#00000";
          type = types.str;
        };
      }) (range 16 99)
    );

  mkColorOption =
    args:
    mkOption (
      args
      // {
        type = types.enum (
          attrNames config.colors ++ attrValues config.colors ++ attrNames config.namedColors
        );
        apply = v: config.colors.${v} or config.namedColors.${v} or v;
      }
    );

  kittyBaseColorOptions = listToAttrs (
    map (i: {
      name = "color${toString i}";
      value = mkColorOption { default = "color${toString i}"; };
    }) (range 0 15)
  );

in
{
  options = {
    name = mkOption {
      type = types.str;
      default = name;
      defaultText = literalExpression "<name>";
    };

    colors = mkOption { type = types.submodule { options = baseColorOptions; }; };

    namedColors = mkOption {
      type = types.attrsOf (types.enum (attrNames config.colors ++ attrValues config.colors));
      default = { };
      apply = mapAttrs (_: v: if hasPrefix "color" v then config.colors.${v} else v);
    };

    terminal = mkOption {
      type = types.submodule {
        options = {
          bg = mkColorOption { };
          fg = mkColorOption { };
          cursorBg = mkColorOption { };
          cursorFg = mkColorOption { };
          selectionBg = mkColorOption { };
          selectionFg = mkColorOption { };
        };
      };
    };

    pkgThemes.kitty = mkOption {
      type = types.submodule {
        options = kittyBaseColorOptions // {
          # Get defaults from `config.terminal`
          background = mkColorOption { default = config.terminal.bg; };
          foreground = mkColorOption { default = config.terminal.fg; };
          cursor = mkColorOption { default = config.terminal.cursorBg; };
          cursor_text_color = mkColorOption { default = config.terminal.cursorFg; };
          selection_background = mkColorOption { default = config.terminal.selectionBg; };
          selection_foreground = mkColorOption { default = config.terminal.selectionFg; };

          url_color = mkColorOption { };
          tab_bar_background = mkColorOption { };
          active_tab_background = mkColorOption { };
          active_tab_foreground = mkColorOption { };
          inactive_tab_foreground = mkColorOption { };
          inactive_tab_background = mkColorOption { };
          bell_border_color = mkColorOption { };
          active_border_color = mkColorOption { default = config.terminal.bg; };
          inactive_border_color = mkColorOption { };
        };
      };
    };

    pkgThemes.alacritty = mkOption {
      type = types.submodule {
        options = {
          primary = mkOption {
            type = types.submodule {
              options = {
                background = mkColorOption { default = config.terminal.bg; };
                foreground = mkColorOption { default = config.terminal.fg; };
              };
            };
          };
          cursor = mkOption {
            type = types.submodule {
              options = {
                text = mkColorOption { default = config.terminal.cursorFg; };
                cursor = mkColorOption { default = config.terminal.cursorBg; };
              };
            };
          };
          selection = mkOption {
            type = types.submodule {
              options = {
                text = mkColorOption { default = config.terminal.selectionFg; };
                background = mkColorOption { default = config.terminal.selectionBg; };
              };
            };
          };
          normal = mkOption {
            type = types.submodule {
              options = {
                black = mkColorOption { default = "color0"; };
                red = mkColorOption { default = "color1"; };
                green = mkColorOption { default = "color2"; };
                yellow = mkColorOption { default = "color3"; };
                blue = mkColorOption { default = "color4"; };
                magenta = mkColorOption { default = "color5"; };
                cyan = mkColorOption { default = "color6"; };
                white = mkColorOption { default = "color7"; };
              };
            };
          };
          bright = mkOption {
            type = types.submodule {
              options = {
                black = mkColorOption { default = "color8"; };
                red = mkColorOption { default = "color9"; };
                green = mkColorOption { default = "color10"; };
                yellow = mkColorOption { default = "color11"; };
                blue = mkColorOption { default = "color12"; };
                magenta = mkColorOption { default = "color13"; };
                cyan = mkColorOption { default = "color14"; };
                white = mkColorOption { default = "color15"; };
              };
            };
          };
        };
      };
    };
  };
}
