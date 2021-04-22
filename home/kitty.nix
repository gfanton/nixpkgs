{ pkgs, lib, ... }: 

let
  xterm-emacsclient = pkgs.writeShellScriptBin "xemacs" ''
    export TERM=xterm-emacs
    ${pkgs.emacs}/bin/emacsclient $@
  '';

  stdin-emacsclient = pkgs.writeShellScriptBin "semacs" ''
    TMP="$(mktemp /tmp/stdin-XXX)"
    cat >$TMP
    ${xterm-emacsclient}/bin/xemacs --nw $TMP
    rm $TMP
  '';
in
{
  programs.mykitty = {
    enable = true;
    package = pkgs.kitty;
    settings = with pkgs.lib.colors.material-colors; {

      # colors
      background = "#${dark1}";
      foreground = "#${dark0}";

      # Cursor
      cursor = "#${dark0}";
      cursor_text_color = "#${dark1}";

      # Selection
      selection_background = "#${dark2}";
      selection_foreground = "#${dark1}";

      # Intactive tab
      tab_bar_background = "#${dark2}";
      active_border_color = "#${green0}";
      active_tab_foreground = "#${green0}";
      active_tab_background = "#${dark1}";

      # Required to use `kitty @` commands
      allow_remote_control = "yes";

      # Colors
      # black
      color0 = "#${black0}";
      color8 = "#${black1}";
      # red
      color1 = "#${red0}";
      color9 = "#${red1}";
      # green
      color2 = "#${green0}";
      color10 = "#${green1}";
      # yellow
      color3 = "#${green0}";
      color11 = "#${green1}";
      # blue
      color4 = "#${blue0}";
      color12 = "#${blue1}";
      # magenta
      color5 = "#${magenta0}";
      color13 = "#${magenta1}";
      # cyan
      color6 = "#${cyan0}";
      color14 = "#${cyan1}";
      # white
      color7 = "#${white0}";
      color15 = "#${white1}";
      # url underline color to fit colors
      url_color = "#${blue0}";

      # config

      font_family = "Iosevka Nerd Font";
      # font_features = "RecMono-Duotone +dlig +ss10";
      font_size = "16.0";
      adjust_line_height = "140%";
      disable_ligatures = "cursor"; # disable ligatures when cursor is on them

      # Window layout
      # hide_window_decorations = "titlebar-only";
      window_padding_width = "10";

      scrollback_pager_history_size = 10;
      macos_option_as_alt = "yes";
      startup_session = "default-session.conf";

      tab_bar_edge = "top";
      tab_bar_style = "powerline";
      tab_title_template = ''{index}: {title}'';
      active_tab_font_style = "bold";
      inactive_tab_font_style = "normal";

      enabled_layouts = "splits";
      enable_audio_bell = "no";
      bell_on_tab = "no";

      kitty_mod = "ctrl+alt";
    };
    keybindings = {

      # open new tab with cmd+t
      "cmd+t" = "new_tab_with_cwd";

      # open new split (window) with cmd+d retaining the cwd
      "cmd+d" = "launch --cwd=current --location=vsplit";
      "cmd+shift+d" = "launch --cwd=current --location=hsplit";

      "cmd+enter" = "toggle_fullscreen";
      "shift+cmd+up" = "move_window up";
      "shift+cmd+left" = "move_window left";
      "shift+cmd+right" = "move_window right";
      "shift+cmd+down" = "move_window down";

      "kitty_mod+left" = "neighboring_window left";
      "kitty_mod+right" = "neighboring_window right";
      "kitty_mod+up" = "neighboring_window up";
      "kitty_mod+down" = "neighboring_window down";

      # clear the terminal screen
      "cmd+k" = "combine : clear_terminal scrollback active : send_text normal,application \x0c";

      # Map cmd + <num> to corresponding tabs
      "cmd+1" = "goto_tab 1";
      "cmd+2" = "goto_tab 2";
      "cmd+3" = "goto_tab 3";
      "cmd+4" = "goto_tab 4";
      "cmd+5" = "goto_tab 5";
      "cmd+6" = "goto_tab 6";
      "cmd+7" = "goto_tab 7";
      "cmd+8" = "goto_tab 8";
      "cmd+9" = "goto_tab 9";

      # changing font sizes
      "kitty_mod+equal" = "change_font_size all +1.0";
      "kitty_mod+minus" = "change_font_size all -1.0";
      "kitty_mod+0" = "change_font_size all 0";

      # screen rollback
      "cmd+f" = "launch --stdin-source=@screen_scrollback --stdin-add-formatting ${stdin-emacsclient}/bin/semacs";

      # hints
      "cmd+g" = "kitten hints --type=linenum --linenum-action=self ${xterm-emacsclient}/bin/xemacs -t +{line} {path}";

      # editor
      "kitty_mod+o" = "launch --cwd=current --type=overlay ${xterm-emacsclient}/bin/xemacs -t .";
      "kitty_mod+e" = "launch --cwd=current --location=vsplit ${xterm-emacsclient}/bin/xemacs -t .";
      "kitty_mod+d" = "launch --cwd=current --location=hsplit ${xterm-emacsclient}/bin/xemacs -t .";
    };
  };

  programs.mykitty.extras.useSymbolsFromNerdFont = "Iosevka Nerd Font";
}
