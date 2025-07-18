{
  config,
  lib,
  pkgs,
  xemacsclient,
  ...
}:

let
  rust_home = "${config.xdg.dataHome}/rust";
in
{
  # ssh
  programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPath = "${config.xdg.cacheHome}/ssh-%u-%r@%h:%p";
    controlPersist = "1800";
    forwardAgent = true;
    serverAliveInterval = 60;
    hashKnownHosts = true;
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
    goPath = ".local/share/go";
    goBin = ".local/bin";
    package = pkgs.pkgs-master.go_1_23;
  };

  # rust env
  # setup cargo home
  home.sessionVariables.CARGO_HOME = "${rust_home}/cargo";
  # setup rustup
  home.sessionVariables.RUSTUP_HOME = "${rust_home}/rustup";
  home.activation.rustup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    export CARGO_HOME="${rust_home}/cargo"
    export RUSTUP_HOME="${rust_home}/rustup"
    ${pkgs.rustup}/bin/rustup toolchain install stable 1>/dev/null
  '';

  home.sessionPath = [
    "${rust_home}/cargo/bin"
    "${rust_home}/rustup/bin"
  ];

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
    ];
    extraConfig = ''
      # Terminal detection and color support
      if "[ $(tput colors) = 16777216 ]" {
        set -g default-terminal "tmux-direct"
      } {
        if "[ $(tput colors) = 256 ]" {
          set -g default-terminal "tmux-256color"
        } {
          set -g default-terminal "tmux"
        }
      }
      
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
      
      # Bind Ctrl-e to run emacsclient in current pane
      bind-key C-e send-keys "cd #{pane_current_path} && TERM=xterm-emacs ${xemacsclient}/bin/xemacsclient -t ." Enter
      # Bind e to open emacsclient in new window
      bind-key e new-window -c "#{pane_current_path}" "TERM=xterm-emacs ${xemacsclient}/bin/xemacsclient -t ."
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
      fzf

      # my
      my-libvterm
      my-loon
      my-gnolint

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

      # rust
      rustup

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

      # go tools
      pkgs-master.gofumpt
      pkgs-master.gopls
      delve
      # exclude bundle
      (pkgs-master.gotools.overrideDerivation (oldAttrs: {
        excludedPackages = [ "bundle" ];
      }))

      # gotools

      # Useful nix related tools
      nixpkgs-fmt
      cachix # adding/managing alternative binary caches hosted by Cachix
      lorri # improve `nix-shell` experience in combination with `direnv`
      niv # easy dependency management for nix projects
      nix-prefetch
      nix-prefetch-git
      nixfmt-rfc-style
    ]
    ++ lib.optionals stdenv.isDarwin [ cocoapods ]
    ++ lib.optionals stdenv.isLinux [
      docker
      docker-compose
    ];
}
