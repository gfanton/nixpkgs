{ config, lib, pkgs, ... }:

let
  inherit (config.home) user-info homeDirectory;
  xterm-emacsclient = pkgs.writeShellScriptBin "xemacsclient" ''
    export TERM=xterm-emacs
    ${pkgs.emacsGcc}/bin/emacsclient $@
  '';
  xterm-emacs = pkgs.writeShellScriptBin "xemacs" ''
    export TERM=xterm-emacs
    ${pkgs.emacsGcc}/bin/emacs $@
  '';
in {
  xdg = {
    enable = true;
    configHome = "${homeDirectory}/.config";
    cacheHome = "${homeDirectory}/.cache";
    dataHome = "${homeDirectory}/.local/share";
  };

  home.sessionPath = [
    # local bin folder
    "${homeDirectory}/.local/bin"
    # npm bin folder
    "${config.xdg.dataHome}/node_modules/bin"
  ];

  home.sessionVariables.LC_ALL = "en_US.UTF-8";
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    zi = {
      enable = true;
      debug = true;
      bin = "${pkgs.zsh-plugins.zi}";
      home = "${config.xdg.configHome}/zi";
      config = ''
        zi ice depth"1"; zi light romkatv/powerlevel10k

        zi light z-shell/zui

        # fzf tab
        zi light Aloxaf/fzf-tab

        # zzcomplete
        zi light z-shell/zzcomplete

        # tab title
        zi wait lucid for trystan2k/zsh-tab-title

        # completion
        zi wait lucid for atload"fast-theme -q default" atinit"ZI[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" z-shell/F-Sy-H
        zi wait lucid for blockf zsh-users/zsh-completions
        zi wait lucid for atload"!_zsh_autosuggest_start" zsh-users/zsh-autosuggestions
      '';
    };

    plugins = [{
      # add powerline10 custom config
      name = "p10k-config";
      src = lib.cleanSource ../config/zsh/p10k;
      file = "config.zsh";
    }];

    # enable completion
    enableCompletion = true;
    enableAutosuggestions = true;

    initExtraFirst = let
      linux = ''
        # -- linux specific config
        # -- linux end
      '';

      darwin = ''
        # -- darwin specific config
        # -- darwin end
      '';

      default = ''
        # -- default config
        # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
        if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
          source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
        fi
        # -- default config end
      '';
      block = [ default ] ++ lib.optionals pkgs.stdenv.isDarwin [ darwin ]
        ++ lib.optionals pkgs.stdenv.isLinux [ linux ];
    in lib.concatStringsSep "\n" block;

    initExtra = let
      linux = ''
        # -- linux specific config
        # -- linux end
      '';

      darwin = ''
        # -- darwin specific config
        # [ -d "$HOME/Library/Android/sdk" ] && export ANDROID_HOME=$HOME/Library/Android/sdk

        # eval brew env
        if [ "$(arch)" = "i386" ] && [ -f /usr/local/bin/brew ]; then
           eval $(/usr/local/bin/brew shellenv)
        elif [ -f /opt/homebrew/bin/brew ]; then # arm64 only
           eval $(/opt/homebrew/bin/brew shellenv)
        fi
        # -- darwin end
      '';

      default = ''
        # -- default config
        # @HOTFIX: set path to local/bin when on i386
        if [ "$(arch)" = "i386" ]; then
           export PATH="/usr/local/opt/openjdk@8/bin:$PATH" # brew java
        fi

        # project
        # if  (( $+commands[project] )) ; then
        #   eval "$(project -debug=false init zsh)" || echo "`project` not found";
        # fi

        # asdf
        . ${pkgs.asdf-vm}/share/asdf-vm/asdf.sh

        # tab-title
        export ZSH_TAB_TITLE_ONLY_FOLDER=true
        export ZSH_TAB_TITLE_ADDITIONAL_TERMS='iterm|kitty'

        # bindkey
        bindkey "\e[1;3D" backward-word # left word
        bindkey "\e[1;3C" forward-word # right word

        ## extra z config

        zstyle ":completion:*:git-checkout:*" sort false
        zstyle ':completion:*:descriptions' format '[%d]'
        zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}

        ## fzf tab
        # cat
        zstyle ':fzf-tab:complete:(cat|bat):*' fzf-preview '\
               ([ -f $realpath ] && ${pkgs.bat}/bin/bat --color=always --style=header,grid --line-range :500 $realpath) \
                || ${pkgs.exa}/bin/exa --color=always --tree --level=1 $realpath'

        # ls
        zstyle ':fzf-tab:complete:cd:*' fzf-preview '${pkgs.exa}/bin/exa --color=always --tree --level=1 $realpath'

        # ps/kill
        # give a preview of commandline arguments when completing `kill`
        zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
        zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
        zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap


        add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

        # https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
        vterm_prompt_end() {
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
        }
        setopt PROMPT_SUBST
        PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

        # -- default end
      '';

      block = [ default ] ++ lib.optionals pkgs.stdenv.isDarwin [ darwin ]
        ++ lib.optionals pkgs.stdenv.isLinux [ linux ];
    in lib.concatStringsSep "\n" block;

    shellAliases = with pkgs; {
      # switch on rosetta shell
      rosetta-zsh = "${pkgs-x86.zsh}/bin/zsh";

      # kitty alias
      ssh = "${kitty}/bin/kitty +kitten ssh";

      # core alias
      ".." = "cd ..";
      cat = "${bat}/bin/bat";
      du = "${du-dust}/bin/dust";
      rg =
        "${ripgrep}/bin/rg --column --line-number --no-heading --color=always --ignore-case";
      ps = "${procs}/bin/procs";
      # npmadd = "${mynodejs}/bin/npm install --global";
      htop = "${btop}/bin/btop";

      # list dir
      ls = "${exa}/bin/exa";
      l = "ls -l --icons";
      la = "l -a";
      ll = "ls -lhmbgUFH --git --icons";
      lla = "ll -a";

      # brew
      #      brew = "${brew}/bin/brew";
      #
      # nix
      config = "make -C ${homeDirectory}/nixpkgs";
    };
  };
}
