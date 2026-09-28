{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs) herdr;
  tomlFormat = pkgs.formats.toml { };

  # Project picker, from the project flake rather than PATH so the binding
  # cannot resolve to a stale build left in the profile.
  projPicker = "${lib.getExe pkgs.proj-herdr} workspace pick";
  projAgentSort = "${lib.getExe pkgs.proj-herdr} agent sort";

  # Built-in themes: catppuccin, catppuccin-latte, terminal, tokyo-night,
  # tokyo-night-day, dracula, nord, gruvbox, gruvbox-light, one-dark, one-light,
  # solarized, solarized-light, kanagawa, kanagawa-lotus, rose-pine,
  # rose-pine-dawn, vesper.
  settings = {
    # herdr shows first-run setup whenever this is missing or true, and it
    # normally writes false itself once onboarding completes. A store-backed
    # config cannot be written, so declaring it is what ends the prompt.
    onboarding = false;

    theme = {
      name = "catppuccin";
      auto_switch = false;
    };

    ui = {
      show_agent_labels_on_pane_borders = true;

      # Order the agents panel as an attention queue rather than grouping it by
      # workspace: blocked first, then finished-but-not-yet-looked-at, then
      # working, then acknowledged. With several agents running at once that
      # puts the ones wanting something from you at the top. prefix+shift+a
      # cycles through two further orderings the config cannot express.
      agent_panel_sort = "priority";

      # Claude Code notifies through its own Stop/Notification hook
      # (~/.claude/hooks/notify.sh, see home/claude-code.nix), which carries a
      # project-scoped title, a body, a signed app icon and a click that jumps
      # back to the originating workspace. herdr's "system" delivery has none of
      # that: with no terminal-notifier on PATH it falls back to `osascript
      # display notification`, which renders unattributed and does nothing on
      # click. Demoting it to an in-app toast keeps a signal for the agents the
      # hook does not cover while taking the system banner away from it.
      toast.delivery = "herdr";

      # The hook already plays a sound for claude, so muting claude alone here
      # avoids a double chime while leaving every other agent audible.
      sound.agents.claude = "off";
    };

    # ---- tmux parity
    # Ported from programs.tmux in home/packages.nix. Where a list is used, the
    # herdr default is kept alongside the tmux key so adding muscle memory costs
    # nothing. Alt chords rely on ghostty's macos-option-as-alt.
    keys = {
      detach = [
        "prefix+q"
        "prefix+d"
      ];
      toggle_sidebar = [
        "prefix+b"
        "prefix+t"
      ];
      new_workspace = [
        "prefix+shift+n"
        "prefix+shift+s"
      ];

      # herdr names splits after the divider, tmux after the resulting layout,
      # so the two are inverted: split_horizontal stacks panes the way tmux's "
      # does.
      split_horizontal = [
        "prefix+minus"
        "prefix+\""
      ];
      split_vertical = [
        "prefix+v"
        "prefix+percent"
      ];

      # tmux binds prefix+; to last-pane. herdr ships the action unbound.
      last_pane = "prefix+semicolon";

      # prefix+e goes to emacs below, matching tmux's bind e.
      edit_scrollback = "prefix+shift+e";

      # theme-switch takes tmux's bind T, so renaming a tab moves off shift+t.
      rename_tab = "prefix+alt+t";

      # Direct alt+1..9 tab switching, replacing tmux's bind -n M-1..M-9.
      # prefix+1..9 keeps working through the switch_tab default.
      indexed.tabs = "alt";

      command = [
        # tmux bind e. emacsclient resolves from the user profile; TERM matches
        # the xemacsclient wrapper in home/emacs.nix, which only exports TERM
        # before exec.
        {
          key = "prefix+e";
          type = "pane";
          command = "TERM=xterm-emacs emacsclient -t .";
          description = "open emacs here";
        }

        # tmux bind T. theme-switch also repaints ghostty, kitty and emacs,
        # which theme.auto_switch cannot do — that setting only restyles herdr's
        # own UI.
        {
          key = "prefix+shift+t";
          type = "shell";
          command = "theme-switch toggle";
          description = "toggle light/dark theme";
        }

        # Project picker, replacing tmux's @proj_popup_key.
        {
          key = "prefix+f";
          type = "popup";
          command = projPicker;
          description = "open project workspace";
          width = "80%";
          height = 20;
        }

        # Agents-panel ordering. herdr holds a custom ordering in the running
        # server with no way to persist or read it back, so this is a keypress
        # rather than a setting: one lap returns to agent_panel_sort above.
        {
          key = "prefix+shift+a";
          type = "shell";
          command = projAgentSort;
          description = "cycle agent panel ordering";
        }
      ];
    };
  };
in
{
  home.packages = [
    herdr
    pkgs.proj-herdr
  ];

  # Generated into the store, so herdr's own settings-UI writes (theme, sound,
  # toast delivery, agent labels, panel sort, onboarding — src/app/config_io.rs)
  # fail EROFS and never persist. herdr degrades to a five-second toast rather
  # than erroring, so the cost of declaring this is that those six settings must
  # be changed here instead of in the app.
  home.file.".config/herdr/config.toml" = {
    source = tomlFormat.generate "herdr-config.toml" settings;

    # The server reads config once at startup, so a rebuild alone leaves a
    # running herdr on the previous keybindings. Activation runs with no server
    # up on a fresh boot, hence the `|| true`.
    onChange = "${lib.getExe herdr} server reload-config || true";
  };
}
