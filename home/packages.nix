{
  config,
  lib,
  pkgs,
  xemacsclient,
  ...
}:

let
  theme-switch = pkgs.writeShellScriptBin "theme-switch" ''
    set -o nounset
    set -o pipefail

    readonly THEME_STATE_FILE="''${XDG_STATE_HOME:-''${HOME}/.local/state}/theme"
    readonly CATPPUCCIN_DIR="${pkgs.tmuxPlugins.catppuccin}/share/tmux-plugins/catppuccin"

    switch_tmux() {
      local flavor="$1"

      if [[ -z "''${TMUX:-}" ]] && ! tmux list-sessions &>/dev/null; then
        return 0
      fi

      # Unset all catppuccin-managed options so re-sourcing applies fresh values.
      # The plugin uses set -ogq, so stale vars block updates.
      local tmux_opts
      tmux_opts="$(tmux show-options -g 2>/dev/null || true)"

      local var
      while IFS= read -r var; do
        [[ -n "''${var}" ]] || continue
        tmux set -Ugq "''${var}"
      done < <(echo "''${tmux_opts}" | grep -oE '@(thm_\w+|catppuccin_status_\w+|_ctp_\w+)' | sort -u)

      tmux set -gq @catppuccin_flavor "''${flavor}"
      tmux run "''${CATPPUCCIN_DIR}/catppuccin.tmux"
    }

    switch_emacs() {
      local mode="$1"
      if ! command -v emacsclient &>/dev/null; then
        return 0
      fi
      emacsclient -e "(my/set-appearance \"''${mode}\")" 2>/dev/null || true
    }

    main() {
      local mode
      case "''${1:-toggle}" in
        dark)  mode="dark" ;;
        light) mode="light" ;;
        toggle)
          local current
          current="$(cat "''${THEME_STATE_FILE}" 2>/dev/null || echo "dark")"
          if [[ "''${current}" == "dark" ]]; then
            mode="light"
          else
            mode="dark"
          fi
          ;;
        *)
          echo "Usage: theme-switch <dark|light|toggle>" >&2
          exit 1
          ;;
      esac

      local flavor
      if [[ "''${mode}" == "dark" ]]; then
        flavor="macchiato"
      else
        flavor="latte"
      fi

      mkdir -p "$(dirname "''${THEME_STATE_FILE}")"
      echo "''${mode}" > "''${THEME_STATE_FILE}"

      switch_tmux "''${flavor}"
      switch_emacs "''${mode}"
    }

    main "$@"
  '';
in
{
  # ssh
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks."*" = {
      controlMaster = "auto";
      controlPath = "${config.xdg.cacheHome}/ssh-%u-%r@%h:%p";
      controlPersist = "1800";
      forwardAgent = true;
      serverAliveInterval = 60;
      hashKnownHosts = true;
    };
    # on darwin use 1password agent
    extraConfig = lib.mkIf pkgs.stdenv.isDarwin ''
      IdentityAgent "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
    '';
  };

  # disable manual
  # Some weird bug
  # https://github.com/NixOS/nixpkgs/issues/196651
  manual.html.enable = false;
  manual.manpages.enable = false;

  # Go Env
  programs.go = {
    enable = true;
    env = {
      GOPATH = "${config.home.homeDirectory}/.local/share/go";
      GOBIN = "${config.home.homeDirectory}/.local/bin";
    };
    package = pkgs.pkgs-master.go_1_25;
  };

  # tmux with catppuccin theme
  programs.tmux = {
    enable = true;
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.catppuccin;
        extraConfig = ''
          set -g @catppuccin_flavor 'macchiato'
          set -g @catppuccin_window_status_style "rounded"
        '';
      }
      {
        plugin = tmuxPlugins.tmux-open-emacs.override {
          xemacsclient = xemacsclient;
        };
        extraConfig = ''
          # Configure tmux-open-emacs plugin
          set -g @toe-open-key 'C-e'
        '';
      }
      {
        plugin = tmuxPlugins.tmux-notify;
        extraConfig = ''
          # tmux-notify plugin config would go here if needed
        '';
      }
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = ''
          set -g @resurrect-capture-pane-contents 'on'
          set -g @resurrect-processes ':all:'
        '';
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '10'
        '';
      }
      {
        # Use pre-packaged tmux plugin from project flake (properly wrapped with binaries)
        plugin = projectTmuxPlugin;
        extraConfig = ''
          set -g @proj_key 'P'
          set -g @proj_popup_key 'C-p'
          set -g @proj_auto_session 'on'
          set -g @proj_show_status 'on'
          set -g @proj_session_format 'proj-#{org}-#{name}'
          set -g @proj_window_format '#{branch}'
        '';
      }
      {
        plugin = tmuxPlugins.tmux-yule-log;
        extraConfig = ''
          set -g @yule-log-idle-time "300"
          set -g @yule-log-mode "fire"
          set -g @yule-log-show-ticker "on"
          set -g @yule-log-lock-enabled "on"
          set -g @yule-log-lock-socket-protect "on"
        '';
      }
    ];
    extraConfig = ''
      # Terminal and color support
      set -g default-terminal "tmux-256color"
      set -ag terminal-overrides ",alacritty:RGB,alacritty:Tc"
      set -ag terminal-overrides ",xterm-256color:RGB,xterm-256color:Tc"
      set -ag terminal-overrides ",*256col*:RGB,*256col*:Tc"
      set -ag terminal-overrides ",xterm-kitty:RGB,xterm-kitty:Tc"
      set -ag terminal-overrides ",xterm-ghostty:RGB,xterm-ghostty:Tc"

      # OSC 52 clipboard support for universal clipboard integration
      set -g set-clipboard on
      set -ag terminal-overrides ",xterm-256color:Ms=\\E]52;%p1%s;%p2%s\\007"
      set -ag terminal-overrides ",xterm-kitty:Ms=\\E]52;%p1%s;%p2%s\\007"
      set -ag terminal-overrides ",xterm-ghostty:Ms=\\E]52;%p1%s;%p2%s\\007"
      set -ag terminal-overrides ",xterm-emacs:Ms=\\E]52;%p1%s;%p2%s\\007"

      # Allow OSC 52 passthrough for nested tmux sessions (tmux 3.3a+)
      set -g allow-passthrough on

      # Update SSH_TTY environment variable on reattach for clipboard after detach/reattach
      set -ag update-environment "SSH_TTY"

      # Enable true color support and other capabilities
      set -sa terminal-features ',alacritty:RGB:usstyle'
      set -sa terminal-features ',xterm-256color:RGB:usstyle'
      set -sa terminal-features ',xterm-kitty:RGB:usstyle'
      set -sa terminal-features ',xterm-ghostty:RGB:usstyle'

      # General settings
      setw -g mode-keys emacs
      set -s escape-time 200
      set -g history-limit 100000
      setw -g aggressive-resize on
      set -g mouse on

      # Window and pane numbering
      set -g base-index 1
      setw -g pane-base-index 1
      setw -g renumber-windows on

      # Status bar
      set -g status-keys emacs
      set -g status-interval 5
      set -g status-position top

      # Window titles
      set -g set-titles on
      set -g set-titles-string "#T"

      # Activity monitoring
      setw -g monitor-activity on
      set -g visual-activity off
      set -g bell-action none

      # Alt+number window selection
      bind-key -n M-1 select-window -t 1
      bind-key -n M-2 select-window -t 2
      bind-key -n M-3 select-window -t 3
      bind-key -n M-4 select-window -t 4
      bind-key -n M-5 select-window -t 5
      bind-key -n M-6 select-window -t 6
      bind-key -n M-7 select-window -t 7
      bind-key -n M-8 select-window -t 8
      bind-key -n M-9 select-window -t 9

      # Window reordering
      bind-key -n M-< swap-window -t -1 \; previous-window
      bind-key -n M-> swap-window -t +1 \; next-window

      # Toggle status bar
      bind-key t set -g status

      # Reload config
      bind-key r source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded"

      # Bind e to open emacsclient in new window
      bind-key e new-window -c "#{pane_current_path}" "TERM=xterm-emacs ${xemacsclient}/bin/xemacsclient -t ."

      # Clear screen with Ctrl+L
      bind-key -n C-l send-keys C-l

      # Split panes with current directory
      bind-key '"' split-window -c "#{pane_current_path}"
      bind-key % split-window -h -c "#{pane_current_path}"

      # New window with current directory
      bind-key c new-window -c "#{pane_current_path}"

      # Create new session with prompted name
      bind-key S command-prompt -p "New session name:" "new-session -s '%%'"

      # ---- Theme Switching
      # Hooks for automatic switching via terminal mode 2031 (tmux 3.6+)
      set-hook -g client-dark-theme  "run-shell '${theme-switch}/bin/theme-switch dark'"
      set-hook -g client-light-theme "run-shell '${theme-switch}/bin/theme-switch light'"

      # Manual toggle keybind (works over SSH)
      bind-key T run-shell '${theme-switch}/bin/theme-switch toggle'
    '';
  };

  # Bat, a substitute for cat.
  # https://github.com/sharkdp/bat
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.bat.enable
  programs.bat.enable = true;

  programs.bat.themes = {
    catppuccin-macchiato = {
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "ba4d16880d63e656acced2b7d4e034e4a93f74b1";
        sha256 = "sha256-6WVKQErGdaqb++oaXnY3i6/GuH2FhTgK0v4TN4Y0Wbw=";
      };
      file = "/Catppuccin-macchiato.tmTheme";
    };
  };

  programs.bat.config = {
    style = "plain";
    theme = "catppuccin-macchiato";
  };

  home.packages =
    with pkgs;
    [
      # Some basics
      tig
      mosh # wrapper for `ssh` that better and not dropping connections
      unrar # extract RAR archives
      eza # fancy version of `ls`
      btop # fancy version of `top`
      tmate # instant terminal sharing
      fd # fancy version of `find`
      most
      parallel # runs commands in parallel
      socat
      lazydocker # The lazier way to manage everything docker
      lazygit # The lazier way to manage everything git
      less
      tree # list contents of directories in a tree-like format.
      coreutils
      jq
      (ripgrep.override { withPCRE2 = true; }) # better version of grep
      curl # transfer a URL
      wget # The non-interactive network downloader.
      entr
      cmake
      gnupg

      # my
      my-libvterm
      my-loon
      my-rtk
      project # From flake input
      theme-switch

      # stable
      procs # fancy version of `ps`

      # aspell
      (aspellWithDicts (d: [
        d.en
        d.fr
        d.en-computers
        d.en-science
      ])) # interactive spell checker
      aspellDicts.fr
      aspellDicts.en
      aspellDicts.en-science
      aspellDicts.en-computers

      # ruby
      ruby

      # js (stable)
      nodejs
      nodePackages.pnpm
      yarn

      # python (stable)
      (python3.withPackages (
        p: with p; [
          virtualenv
          pip
          mypy
          pylint
          yapf
          setuptools
        ]
      ))
      pkgs-stable.pipenv

      # direnv + devenv
      devenv
      direnv

      # go tools (gopls + gofumpt provided by myEmacsDevPackages)
      delve
      # exclude bundle
      (pkgs-master.gotools.overrideDerivation (oldAttrs: {
        excludedPackages = [ "bundle" ];
      }))

      # Useful nix related tools
      nixpkgs-fmt
      cachix # adding/managing alternative binary caches hosted by Cachix
      lorri # improve `nix-shell` experience in combination with `direnv`
      niv # easy dependency management for nix projects
      nix-prefetch
      nix-prefetch-git
      nixfmt-rfc-style
    ]
    ++ lib.optionals stdenv.isDarwin [
      cocoapods
      colima # Container runtime for macOS
      docker-client # Docker CLI for Colima
    ]
    ++ lib.optionals stdenv.isLinux [
      docker
      docker-compose
      colima # Container runtime for Linux
      util-linux # Provides arch, lscpu, and other utilities (native on Darwin)
    ];
}
