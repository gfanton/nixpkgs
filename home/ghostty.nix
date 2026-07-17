{ pkgs, ... }:

{
  programs.ghostty = {
    enable = true;

    package = if pkgs.stdenv.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;

    enableZshIntegration = true;

    settings = {
      font-family = "JetBrainsMono Nerd Font Mono";
      font-size = 14;

      theme = "light:Catppuccin Latte,dark:Catppuccin Macchiato";

      background-opacity = 0.85;
      macos-titlebar-style = "native";
      macos-option-as-alt = true;
      auto-update = "off";
      quit-after-last-window-closed = true;
      copy-on-select = "clipboard";
      clipboard-read = "allow";
      clipboard-write = "allow";

      # Install Ghostty's terminfo on remote hosts over SSH and fall back to
      # TERM=xterm-256color when it can't, so remote TUIs work without a manual
      # `TERM=xterm`. Merges onto the default feature set (cursor,title,path).
      shell-integration-features = "ssh-env,ssh-terminfo";

      mouse-hide-while-typing = true;
      cursor-style-blink = false;
      grapheme-width-method = "legacy";

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
        # Window management. cmd+t opens a Ghostty tab (internal to one macOS
        # window, so yabai still tiles it as a single window); cmd+enter opens
        # a separate window.
        "cmd+t=new_tab"
        "cmd+enter=new_window"

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
