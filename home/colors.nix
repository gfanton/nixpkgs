{ config, ... }:

{
  colors.catppuccin-macchiato = {
    colors = {
      # black
      color0 = "#494d64"; # Default Black / surface1
      color8 = "#5b6078"; # Bright Black / surface2
      # red
      color1 = "#ed8796"; # Default Red / Red
      color9 = "#ee99a0"; # Bright Red / Maroon
      # green
      color2 = "#a6da95"; # Default Green / Green
      color10 = "#8bd5ca"; # Bright Green / Teal
      # yellow
      color3 = "#f5a97f"; # Default Yellow / Peach
      color11 = "#eed49f"; # Bright Yellow / Yellow
      # blue
      color4 = "#8aadf4"; # Default Blue / Blye
      color12 = "#b7bdf8"; # Bright Blue / Lavender
      # megenta
      color5 = "#f5bde6"; # Default Magenta / Pink
      color13 = "#c6a0f6"; # Bright Magenta / Mauve
      # cyan
      color6 = "#91d7e3"; # Default Cyan / Sky
      color14 = "#7dc4e4"; # Bright Cyan / Sapphire
      # white
      color7 = "#f4dbd6"; # Default White / Rosewater
      color15 = "#f0c6c6"; # Bright White / Flamingo

      # other
      color16 = "#cad3f5"; # text
      color17 = "#24273a"; # base
      color18 = "#1e2030"; # mantle
      color19 = "#181926"; # crust
      color20 = "#494d64"; # surface

      color21 = "none"; # surface
    };

    namedColors = {
      # material name
      black = "color0";
      brightBlack = "color8";
      red = "color1";
      brightRed = "color9";
      green = "color2";
      brightGreen = "color10";
      yellow = "color3";
      brightYellow = "color11";
      blue = "color4";
      brightBlue = "color12";
      magenta = "color5";
      brightMagenta = "color13";
      cyan = "color6";
      brightCyan = "color14";
      white = "color7";
      brightWhite = "color15";

      # other
      text = "color16";
      base = "color17";
      mantle = "color18";
      crust = "color19";
      surface = "color20";

      none = "color21";
    };

    terminal = {
      bg = "base";
      fg = "text";
      cursorBg = "white";
      cursorFg = "black";
      selectionBg = "white";
      selectionFg = "black";
    };

    pkgThemes.kitty = {
      url_color = "blue";
      tab_bar_background = "none";
      active_tab_background = "yellow";
      active_tab_foreground = "black";
      inactive_tab_background = "crust";
      inactive_tab_foreground = "text";
      active_border_color = "yellow";
      inactive_border_color = "surface";
      bell_border_color = "brightBlue";
    };

    pkgThemes.alacritty = {
      primary = {
        background = "base";
        foreground = "text";
      };
      cursor = {
        text = "base";
        cursor = "white";
      };
      selection = {
        text = "base";
        background = "white";
      };
      normal = {
        black = "color0";
        red = "color1";
        green = "color2";
        yellow = "color11";
        blue = "color4";
        magenta = "color5";
        cyan = "color10";
        white = "color7";
      };
      bright = {
        black = "color8";
        red = "color1";
        green = "color2";
        yellow = "color11";
        blue = "color4";
        magenta = "color5";
        cyan = "color10";
        white = "color15";
      };
    };
  };

  colors.catppuccin-latte = {
    colors = {
      # black
      color0 = "#bcc0cc"; # Default Black / surface1
      color8 = "#acb0be"; # Bright Black / surface2
      # red
      color1 = "#d20f39"; # Default Red / Red
      color9 = "#d20f39"; # Bright Red / Maroon
      # green
      color2 = "#40a02b"; # Default Green / Green
      color10 = "#179299"; # Bright Green / Teal
      # yellow
      color3 = "#fe640b"; # Default Yellow / Peach
      color11 = "#df8e1d"; # Bright Yellow / Yellow
      # blue
      color4 = "#1e66f5"; # Default Blue / Blue
      color12 = "#7287fd"; # Bright Blue / Lavender
      # megenta
      color5 = "#ea76cb"; # Default Magenta / Pink
      color13 = "#8839ef"; # Bright Magenta / Mauve
      # cyan
      color6 = "#04a5e5"; # Default Cyan / Sky
      color14 = "#209fb5"; # Bright Cyan / Sapphire
      # white
      color7 = "#dc8a78"; # Default White / Rosewater
      color15 = "#dd7878"; # Bright White / Flamingo

      # other
      color16 = "#4c4f69"; # text
      color17 = "#eff1f5"; # base
      color18 = "#e6e9ef"; # mantle
      color19 = "#dce0e8"; # crust
      color20 = "#ccd0da"; # surface

      color21 = "none";
    };

    namedColors = {
      # material name
      black = "color0";
      brightBlack = "color8";
      red = "color1";
      brightRed = "color9";
      green = "color2";
      brightGreen = "color10";
      yellow = "color3";
      brightYellow = "color11";
      blue = "color4";
      brightBlue = "color12";
      magenta = "color5";
      brightMagenta = "color13";
      cyan = "color6";
      brightCyan = "color14";
      white = "color7";
      brightWhite = "color15";

      # other
      text = "color16";
      base = "color17";
      mantle = "color18";
      crust = "color19";
      surface = "color20";

      none = "color21";
    };

    terminal = {
      bg = "base";
      fg = "text";
      cursorBg = "white";
      cursorFg = "black";
      selectionBg = "white";
      selectionFg = "black";
    };

    pkgThemes.kitty = {
      url_color = "blue";
      tab_bar_background = "none";
      active_tab_background = "yellow";
      active_tab_foreground = "black";
      inactive_tab_background = "crust";
      inactive_tab_foreground = "text";
      active_border_color = "yellow";
      inactive_border_color = "surface";
      bell_border_color = "brightBlue";
    };

    pkgThemes.alacritty = {
      primary = {
        background = "base";
        foreground = "text";
      };
      cursor = {
        text = "base";
        cursor = "white";
      };
      selection = {
        text = "base";
        background = "white";
      };
      normal = {
        black = "color0";
        red = "color1";
        green = "color2";
        yellow = "color11";
        blue = "color4";
        magenta = "color5";
        cyan = "color10";
        white = "color7";
      };
      bright = {
        black = "color8";
        red = "color1";
        green = "color2";
        yellow = "color11";
        blue = "color4";
        magenta = "color5";
        cyan = "color10";
        white = "color15";
      };
    };
  };

  colors.material = {
    colors = {
      # black
      color0 = "#546e7a"; # Default Black
      color8 = "#b0bec5"; # Bright Black
      # red
      color1 = "#ff5252"; # Default Red
      color9 = "#ff8a80"; # Bright Red
      # green
      color2 = "#5cf19e"; # Default Green
      color10 = "#b9f6ca"; # Bright Green
      # yellow
      color3 = "#ffd740"; # Default Yellow
      color11 = "#ffe57f"; # Bright Yellow
      # blue
      color4 = "#40c4ff"; # Default Blue
      color12 = "#80d8ff"; # Bright Blue
      # megenta
      color5 = "#ff4081"; # Default Magenta
      color13 = "#ff80ab"; # Bright Magenta
      # cyan
      color6 = "#64fcda"; # Default Cyan
      color14 = "#a7fdeb"; # Bright Cyan
      # white
      color7 = "#ffffff"; # Default White
      color15 = "#ffffff"; # Bright White

      # other
      color16 = "#eceff1"; # Bright Gray
      color17 = "#263238"; # Gunmetal
      color18 = "#607d8b"; # Steel Teal
    };

    namedColors = {
      # material name
      black = "color0";
      brightBlack = "color8";
      red = "color1";
      brightRed = "color9";
      green = "color2";
      brightGreen = "color10";
      yellow = "color3";
      brightYellow = "color11";
      blue = "color4";
      brightBlue = "color12";
      magenta = "color5";
      brightMagenta = "color13";
      cyan = "color6";
      brightCyan = "color14";
      white = "color7";
      brightWhite = "color15";

      # other
      brightGray = "color16";
      gunmetal = "color17";
      steelTeal = "color18";
    };

    terminal = {
      bg = "base";
      fg = "text";
      cursorBg = "white";
      cursorFg = "black";
      selectionBg = "white";
      selectionFg = "black";
    };

    pkgThemes.kitty = {
      url_color = "blue";
      tab_bar_background = "black";
      active_tab_background = "gunmetal";
      active_tab_foreground = "green";
      inactive_tab_background = "black";
      inactive_tab_foreground = "brightGreen";
    };
  };
}
