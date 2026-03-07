{ pkgs, ... }:

{
  programs.ghostty = {
    enable = true;

    package =
      if pkgs.stdenv.isDarwin then pkgs.ghostty-bin
      else pkgs.ghostty;

    enableZshIntegration = true;

    settings = {
      font-family = "JetBrainsMono Nerd Font Mono";
      font-size = 14;

      theme = "light:Catppuccin Latte,dark:Catppuccin Macchiato";

      background-opacity = 0.85;
      macos-titlebar-style = "hidden";
      macos-option-as-alt = true;
      auto-update = "off";
      quit-after-last-window-closed = true;
      copy-on-select = "clipboard";
      clipboard-read = "allow";
      clipboard-write = "allow";
      mouse-hide-while-typing = true;
      cursor-style-blink = false;

      window-padding-x = 5;
      window-padding-y = 5;

      font-feature = [
        "+cv02"
        "+cv05"
        "+cv09"
        "+cv14"
        "+ss04"
        "+cv16"
        "+cv31"
        "+cv25"
        "+cv26"
        "+cv32"
        "+cv28"
        "+ss10"
        "+zero"
        "+onum"
      ];

      keybind = [
        # Tab management
        "cmd+t=new_tab"
        "cmd+enter=new_window"

        # Tab navigation
        "cmd+one=goto_tab:1"
        "cmd+two=goto_tab:2"
        "cmd+three=goto_tab:3"
        "cmd+four=goto_tab:4"
        "cmd+five=goto_tab:5"
        "cmd+six=goto_tab:6"
        "cmd+seven=goto_tab:7"
        "cmd+eight=goto_tab:8"
        "cmd+nine=goto_tab:9"

        # Split navigation (vim-style)
        "ctrl+alt+h=goto_split:left"
        "ctrl+alt+j=goto_split:bottom"
        "ctrl+alt+k=goto_split:top"
        "ctrl+alt+l=goto_split:right"

        # Split creation
        "cmd+d=new_split:right"
        "cmd+shift+d=new_split:down"

        # Clear screen
        "cmd+k=clear_screen"
      ];
    };
  };
}
