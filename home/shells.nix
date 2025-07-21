{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.home) user-info homeDirectory;
  configDir = ".config";
  cacheDir = ".cache";
  dataDir = ".local/share";
  oh-my-zsh-custom = "${configDir}/oh-my-zsh";

  treeSitterLang = pkgs.tree-sitter.withPlugins (p: [
    p.tree-sitter-typescript
    p.tree-sitter-javascript
    p.tree-sitter-templ
    p.tree-sitter-go
    p.tree-sitter-gomod
    p.tree-sitter-gowork
  ]);

  treeSitterLangRenamed = pkgs.runCommand "tree-sitter-lang-renamed" { } ''
    mkdir -p $out
    for file in ${treeSitterLang}/*.so; do
      cp $file $out/libtree-sitter-$(basename $file)
    done
  '';

  xterm-emacsclient = pkgs.writeShellScriptBin "xemacsclient" ''
    export TERM=xterm-emacs
    ${pkgs.emacs-gtk}/bin/emacsclient $@
  '';
  xterm-emacs = pkgs.writeShellScriptBin "xemacs" ''
    export TERM=xterm-emacs
    ${pkgs.emacs-gtk}/bin/emacs $@
  '';

  restart-service = pkgs.writeShellScriptBin "restart-service" ''
    set -e

    plist_name="$1"
    plist_path=$(find $HOME/Library/LaunchAgents -name "$plist_name" 2>/dev/null | head -n 1)

    if [[ -z "$plist_path" ]]; then
        echo "Unable to find $plist_name"
        exit 1
    fi

    echo "restarting $plist_name"

    major_version=$(sw_vers -productVersion | cut -d. -f2)

    if [[ $major_version -ge 16 ]]; then
        # For macOS Big Sur and later
        launchctl bootout system "$plist_path" && launchctl bootstrap system "$plist_path"
    else
        # For macOS Catalina and earlier
        launchctl unload "$plist_path" && launchctl load "$plist_path"
    fi
  '';

in
{
  xdg = {
    enable = true;
    configHome = "${homeDirectory}/${configDir}";
    cacheHome = "${homeDirectory}/${cacheDir}";
    dataHome = "${homeDirectory}/${dataDir}";
  };

  home.sessionPath = [
    # local bin folder
    "${homeDirectory}/.local/bin"
    # npm bin folder
    "${config.xdg.dataHome}/node_modules/bin"
  ];

  home.sessionVariables = {
    LC_ALL = "en_US.UTF-8";
    # XXX: move this elsewhere
    TREE_SITTER_LANG = treeSitterLangRenamed;
  };

  # Direnv, load and unload environment variables depending on the current directory.
  # https://direnv.net
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.direnv.enable
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # Htop
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.htop.enable
  programs.htop.enable = true;
  programs.htop.settings.show_program_path = true;

  # Zoxide, a faster way to navigate the filesystem
  # https://github.com/ajeetdsouza/zoxide
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.zoxide.enable
  programs.zoxide.enable = true;

  # Modern fzf configuration using home-manager
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "fd --type f --hidden --follow --exclude .git";
    changeDirWidgetCommand = "fd --type d --hidden --follow --exclude .git";
    historyWidgetOptions = [
      "--preview 'echo {}'"
      "--preview-window down:3:wrap"
      "--bind 'ctrl-y:execute-silent(echo {2..} | pbcopy)'"
      "--header 'CTRL-Y: copy command to clipboard'"
      "--exact"
    ];
    fileWidgetOptions = [
      "--preview 'bat --color=always --style=numbers --line-range=:500 {}'"
      "--preview-window=right:60%:wrap"
      "--bind 'ctrl-y:execute-silent(echo {} | pbcopy)'"
      "--header 'CTRL-Y: copy path to clipboard'"
    ];
    changeDirWidgetOptions = [
      "--preview 'eza --tree --level=2 --color=always {}'"
      "--preview-window=right:60%:wrap"
    ];
    defaultOptions = [
      "--height=40%"
      "--layout=reverse"
      "--border=rounded"
      "--margin=1"
      "--padding=1"
      "--info=inline"
      "--prompt='❯ '"
      "--pointer='▶'"
      "--marker='✓'"
      "--color=fg:#908caa,bg:#191724,hl:#ebbcba"
      "--color=fg+:#e0def4,bg+:#26233a,hl+:#f6c177"
      "--color=border:#403d52,header:#31748f,gutter:#191724"
      "--color=spinner:#f6c177,info:#9ccfd8,separator:#403d52"
      "--color=pointer:#c4a7e7,marker:#eb6f92,prompt:#908caa"
      "--bind=tab:accept,shift-tab:up,ctrl-j:down,ctrl-k:up"
      "--bind=ctrl-u:half-page-up,ctrl-d:half-page-down"
      "--bind=ctrl-space:toggle,ctrl-a:select-all,ctrl-alt-a:deselect-all"
      "--cycle"
    ];
  };

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    plugins = [
      {
        # add powerline10 custom config
        name = "p10k-config";
        src = lib.cleanSource ../config/zsh/p10k;
        file = "config.zsh";
      }
    ];

    # enable completion
    enableCompletion = true;
    autosuggestion.enable = true;
    oh-my-zsh = {
      enable = true;
      custom = "${config.xdg.configHome}/oh-my-zsh";
      extras = {
        themes = [
          {
            name = "powerlevel10k";
            source = pkgs.zsh-plugins.powerlevel10k;
          }
        ];
        plugins = [
          {
            name = "fzf-tab";
            source = pkgs.zsh-plugins.fzf-tab;
            config = ''
              # fzf-tab configuration following official Aloxaf/fzf-tab best practices
              # https://github.com/Aloxaf/fzf-tab
              
              # ESSENTIAL: Core fzf-tab configuration 
              # Disable menu globally to allow fzf-tab to work (required)
              zstyle ':completion:*' menu no
              zstyle ':completion:*:descriptions' format '[%d]'
              zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}
              
              # PERFORMANCE: Disable sort for git operations (recommended by fzf-tab)
              zstyle ':completion:*:git-checkout:*' sort false
              
              # PREVIEW: Directory navigation (official recommendation)
              zstyle ':fzf-tab:complete:cd:*' fzf-preview '${pkgs.eza}/bin/eza -1 --color=always $realpath'
              
              # PREVIEW: File viewing with fallback
              zstyle ':fzf-tab:complete:(cat|bat|less|more|head|tail):*' fzf-preview ' \
                ${pkgs.bat}/bin/bat --color=always --style=numbers --line-range :500 $realpath 2>/dev/null || \
                ${pkgs.eza}/bin/eza --color=always $realpath'
              
              # PREVIEW: Process management (kill/ps commands)
              zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm,cmd -w -w"
              zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
                '[[ $group == "[process ID]" ]] && ps --pid=$word -o pid,ppid,user,comm,cmd --no-headers -w -w'
              zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
              
              # PREVIEW: Git operations
              zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
                'git diff --color=always $word 2>/dev/null || git status --short'
              zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
                'git log --color=always --oneline --graph $word'
              zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
                'git show --color=always $word'
              zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
                'case "$group" in \
                  "modified file") git diff --color=always $word;; \
                  "recent commit object name") git show --color=always $word;; \
                  *) git log --color=always --oneline $word;; \
                esac'
              
              # PREVIEW: Environment variables
              zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
                fzf-preview 'echo ''${(P)word}'
              
              # PREVIEW: Manual pages
              zstyle ':fzf-tab:complete:(man|help):*' fzf-preview \
                'man $word 2>/dev/null | head -200'
              
              # NAVIGATION: Group switching (F1/F2 alternative)
              zstyle ':fzf-tab:*' switch-group ',' '.'
              
              # NAVIGATION: Continuous trigger
              zstyle ':fzf-tab:*' continuous-trigger '/'
              
              # APPEARANCE: Modern color scheme and bindings
              zstyle ':fzf-tab:*' fzf-flags \
                --color=fg:#908caa,bg:#191724,hl:#ebbcba \
                --color=fg+:#e0def4,bg+:#26233a,hl+:#f6c177 \
                --color=border:#403d52,header:#31748f \
                --color=pointer:#c4a7e7,marker:#eb6f92,prompt:#908caa \
                --bind=tab:accept,shift-tab:up \
                --bind=ctrl-space:toggle \
                --cycle
              
              # OPTIMIZATION: Enable prefix matching for better performance
              zstyle ':fzf-tab:*' prefix ""
            '';
          }
          {
            name = "fast-syntax-highlighting";
            source = pkgs.zsh-plugins.fast-syntax-highlighting;
          }
        ];
      };
      theme = "powerlevel10k/powerlevel10k";
      plugins =
        [
          "sudo"
          "git"
          "fzf"
          "zoxide"
          "cp"
        ]
        # ++ [ "fzf-tab" "fast-syntax-highlighting" ] # extra plugins list
        ++ lib.optionals pkgs.stdenv.isDarwin [
          "brew"
          "macos"
        ]
        ++ lib.optionals pkgs.stdenv.isLinux [ ];
    };

    initExtra = lib.mkMerge [
      (lib.mkBefore ''
        # Powerlevel10k instant prompt will be enabled by the theme
      '')
      (lib.mkAfter ''
        # Shell environment configuration
        export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE=fg:240
        export ZSH_TAB_TITLE_ONLY_FOLDER=true
        export ZSH_TAB_TITLE_ADDITIONAL_TERMS=iterm

        # Additional completion configuration 
        zstyle ":completion:*:git-checkout:*" sort false
        zstyle ':completion:*:descriptions' format '[%d]'
        zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"

        # Project completion configuration
        # Option 1: Disable fzf-tab for p command to use native zsh completion
        zstyle ':fzf-tab:complete:p:*' disabled-on any
        zstyle ':completion:*:p:*' menu yes select interactive
        
        # Option 2: Enhanced fzf-tab integration for p command (alternative)
        # zstyle ':fzf-tab:complete:p:*' fzf-preview 'echo "Navigate to: {}"'
        # zstyle ':fzf-tab:complete:p:*' fzf-flags --header="Project Navigation" --height=60%
        
        # Load project shell integration if available
        if command -v proj >/dev/null 2>&1; then
          eval "$(proj init zsh)"
        fi

        # tmux shell integration functions (adapted from https://chadaustin.me/2024/02/tmux-config/)
        if [[ "$TMUX" ]]; then
            function lv() {
                tmux split-window -h less "$@"
            }
            function ev() {
                tmux split-window -h ''${xterm-emacsclient}/bin/xemacsclient "$@"
            }
            function lh() {
                tmux split-window -v less "$@"
            }
            function eh() {
                tmux split-window -v ''${xterm-emacsclient}/bin/xemacsclient "$@"
            }
        fi
      '')
    ];

    shellAliases =
      with pkgs;
      let
        ezaTree = lib.listToAttrs (
          map (i: {
            name = "ls${toString i}";
            value = "ls -T --level=${toString i}";
          }) (lib.range 0 10)
        );
        ezaTreelist = lib.listToAttrs (
          map (i: {
            name = "l${toString i}";
            value = "ls -T --level=${toString i} -l";
          }) (lib.range 0 10)
        );
      in
      {
        dev = "(){ nix develop $1 -c $SHELL ;}";
        mydev = "(){ nix develop my#$1 -c $SHELL ;}";

        # emacs defined in emacs.nix
        ec = "emacsclient";

        # kitty alias
        ssh = "${pkgs.pkgs-stable.kitty}/bin/kitten ssh";

        # core alias
        ".." = "cd ..";
        cat = "${bat}/bin/bat";
        du = "${du-dust}/bin/dust";
        rg = "${ripgrep}/bin/rg --column --line-number --no-heading --color=always --ignore-case";
        ps = "${procs}/bin/procs";
        # npmadd = "${mynodejs}/bin/npm install --global";
        htop = "${btop}/bin/btop";

        # list dir
        ls = "${eza}/bin/eza";
        l = "ls -l --icons";
        la = "l -a";
        ll = "ls -lhmbgUFH --git --icons";
        lla = "ll -a";
        config = "make -C ${homeDirectory}/nixpkgs";
      }
      // ezaTree
      // ezaTreelist
      // (lib.optionalAttrs (stdenv.system == "aarch64-darwin") {
        # switch on rosetta shell
        rosetta-zsh = "${pkgs-x86.zsh}/bin/zsh";

        # yabai & skhd
        restart-yabai = "${restart-service}/bin/restart-service org.nixos.yabai.plist";
        restart-skhd = "${restart-service}/bin/restart-service org.nixos.skhd.plist";
        restart-borders = "${restart-service}/bin/restart-service org.nixos.jankyborders.plist";
      });
  };
}
