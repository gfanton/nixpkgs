{
  config,
  lib,
  pkgs,
  xemacsclient,
  ...
}:

let
  stdin-emacsclient = pkgs.writeShellScriptBin "semacs" ''
    TMP="$(mktemp /tmp/stdin-XXXX)"
    cat > $TMP.ansi
    ${xemacsclient}/bin/xemacsclient -t $TMP.ansi
    rm $TMP*
  '';

  magit-emacsclient = pkgs.writeShellScriptBin "magit" ''
    ${xemacsclient}/bin/xemacsclient -t -e '(magit-status) (delete-other-windows)'
  '';

  scratch-emacsclient = pkgs.writeShellScriptBin "scratch" ''
    ${xemacsclient}/bin/xemacsclient -t -e '(spacemacs/switch-to-scratch-buffer) (delete-other-windows) (evil-emacs-state)'
  '';

  theme-dark = config.colors.catppuccin-macchiato;
  theme-light = config.colors.catppuccin-latte;
in
{
  # Alacritty terminal
  # https://alacritty.org/config-alacritty.html
  # https://nix-community.github.io/home-manager/options.html#opt-programs.alacritty.enable
  programs.alacritty.enable = true;

  # Enable truecolor support for Alacritty (conditional to avoid conflict with Kitty)
  programs.truecolor.enable = true;

  programs.alacritty.settings = {
    # Font configuration
    font = {
      normal = {
        family = "JetBrainsMono Nerd Font Mono";
      };
      size = 14.0;
    };

    # Window configuration
    window = {
      padding = {
        x = 5;
        y = 5;
      };
      decorations = "buttonless"; # macOS equivalent to kitty's titlebar-only
      opacity = 0.85;
      startup_mode = "Windowed";
    };

    # Terminal behavior
    scrolling = {
      history = 100000;
    };

    # Selection
    selection = {
      save_to_clipboard = true;
    };

    # General settings
    general = {
      live_config_reload = true;
    };

    # Environment
    env = {
      TERM = "alacritty";
      TERMINFO_DIRS = "/etc/profiles/per-user/gfanton/share/terminfo:/usr/share/terminfo";
      COLORTERM = "truecolor";
    };

    # Terminal settings for better color accuracy
    terminal = {
      osc52 = "CopyPaste"; # Enable OSC 52 for clipboard
    };

    # Colors (using dark theme by default)
    colors = theme-dark.pkgThemes.alacritty;

    # Keyboard bindings
    keyboard.bindings = [
      # New tab (similar to kitty cmd+t)
      {
        key = "T";
        mods = "Command";
        action = "CreateNewWindow";
      }

      # New window
      {
        key = "Return";
        mods = "Command";
        action = "CreateNewWindow";
      }

      # Clear screen (similar to kitty cmd+k)
      {
        key = "K";
        mods = "Command";
        chars = "\f";
      }

      # Note: Ctrl+L removed to let tmux handle it properly

      # Font size controls
      {
        key = "Equals";
        mods = "Control|Alt";
        action = "IncreaseFontSize";
      }
      {
        key = "Minus";
        mods = "Control|Alt";
        action = "DecreaseFontSize";
      }
      {
        key = "Key0";
        mods = "Control|Alt";
        action = "ResetFontSize";
      }

      # Copy/Paste
      {
        key = "C";
        mods = "Command";
        action = "Copy";
      }
      {
        key = "V";
        mods = "Command";
        action = "Paste";
      }

      # Quit
      {
        key = "Q";
        mods = "Command";
        action = "Quit";
      }

      # Hide window
      {
        key = "H";
        mods = "Command";
        action = "Hide";
      }

      # Toggle fullscreen
      {
        key = "F";
        mods = "Command|Shift";
        action = "ToggleFullscreen";
      }
    ];
  };
}
