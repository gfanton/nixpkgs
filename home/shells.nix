{ config, pkgs, lib, ... }:

let
  zshPlugins = "${config.programs.zsh.dotDir}/zsh-plugins.sh";
  zshPluginsSource = "${lib.cleanSource ../config/zsh/zsh-plugins.txt}";

  xterm-emacsclient = pkgs.writeShellScriptBin "xemacsclient" ''
    export TERM=xterm-emacs
    ${pkgs.emacs}/bin/emacsclient $@
  '';

  xterm-emacs = pkgs.writeShellScriptBin "xemacs" ''
    export TERM=xterm-emacs
    ${pkgs.emacs}/bin/emacs $@
  '';
in
{
  home.activation.generateAntibodyPlugins = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    ${pkgs.silicon.antibody}/bin/antibody bundle < "${zshPluginsSource}" > "${zshPlugins}";
  '';

  programs.tmux.enable = true;

  # fzf - a command-line fuzzy finder.
  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  # ZSH
  programs.zsh = {
    enable = true;

    # zsh config location
    dotDir = ".config/zsh";

    # enable completion
    enableCompletion = true;

    # enable completion
    enableAutosuggestions = true;

    plugins = [
      {
        # add powerline10 custom config
        name = "p10k-config";
        src = lib.cleanSource ../config/zsh/p10k;
        file = "config.zsh";
      }

      # {
      #   # fzf custom config
      #   name = "fzf-config";
      #   src = lib.cleanSource ../config/zsh/fzf;
      #   file = "config.zsh";
      # }
    ];

    initExtraFirst = ''
    # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
    if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
      source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
    fi
    '';

    initExtra = ''
    # source antibody plugins
    if [ -f "${config.home.homeDirectory}/${zshPlugins}" ]; then
      source "${config.home.homeDirectory}/${zshPlugins}"
    fi

    # bindkey
    bindkey "\e[1;3D" backward-word # left word
    bindkey "\e[1;3C" forward-word # right word

    ## extra z config
    # fsh
    fast-theme -q default

    # Do menu-driven completion.
    zstyle ":completion:*:git-checkout:*" sort false
    zstyle ':completion:*:descriptions' format '[%d]'
    zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}

    ## fzf tab

    # zoxide
    zstyle ':completion:z:*' command 'zq -ls'

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
    '';

    shellAliases = with pkgs; {
      # kitty alias
      ssh = "${kitty}/bin/kitty +kitten ssh";

      # core alias
      ".." = "cd ..";
      cat = "${bat}/bin/bat";
      du = "${du-dust}/bin/dust";
      g = "${gitAndTools.git}/bin/git";
      ls = "${exa}/bin/exa";
      l = "ls -l --icons";
      la = "l -a --icons";
      ll = "ls -lhmbgUFH --git --icons";
      lla = "ll -a";
      ps = "${stable.procs}/bin/procs";

      # emacs
      emacs = "${xterm-emacs}/bin/xemacs";
      emacsclient = "${xterm-emacsclient}/bin/xemacsclient";
      ec = "${xterm-emacsclient}/bin/xemacsclient -nw";
    };
  };
}
