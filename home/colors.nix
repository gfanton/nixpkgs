{ config, ... }:

{
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
      bg = "gunmetal";
      fg = "white";
      cursorBg = "white";
      cursorFg = "black";
      selectionBg = "steelTeal";
      selectionFg = "brightGray";
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
