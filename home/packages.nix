{ config, lib, pkgs, ... }:

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
  };

  # Go Env
  programs.go = {
    enable = true;
    goPath = ".local/share/go/18";
    goBin = ".local/bin";
    package = pkgs.go_1_18;
  };

  # Bat, a substitute for cat.
  # https://github.com/sharkdp/bat
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.bat.enable
  programs.bat.enable = true;
  programs.bat.config = { style = "plain"; };

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

  home.packages = with pkgs;
    [
      # Some basics
      mosh # wrapper for `ssh` that better and not dropping connections
      unrar # extract RAR archives
      exa # fancy version of `ls`
      # stable.bandwhich # display current network utilization by process

      # pkgs silicon
      btop # fancy version of `top`
      tmate # instant terminal sharing
      fd # fancy version of `find`
      most
      parallel # runs commands in parallel
      socat
      less
      tree # list contents of directories in a tree-like format.
      coreutils
      jq
      (ripgrep.override { withPCRE2 = true; }) # better version of grep
      curl # transfer a URL
      wget # The non-interactive network downloader.
      asdf-vm
      entr
      cmake
      gnupg

      # my
      my-libvterm
      my-loon

      # stable
      procs # fancy version of `ps`

      # aspell
      aspell # interactive spell checker
      aspellDicts.fr
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science

      # rustc
      rustc
      cargo

      # ruby
      (ruby_2_7.withPackages (ps: [ ps.ffi-compiler ]))
      # js
      nodejs-16_x
      yarn

      # python
      (python39.withPackages
        (p: with p; [ virtualenv pip mypy pylint yapf setuptools ]))
      pipenv

      # go tools
      gofumpt
      gopls # see overlay
      delve
      # exclude bundle
      (gotools.overrideDerivation (oldAttrs: {
        excludedPackages = oldAttrs.excludedPackages ++ [ "bundle" ];
      }))

      # Useful nix related tools
      cachix # adding/managing alternative binary caches hosted by Cachix
      lorri # improve `nix-shell` experience in combination with `direnv`
      niv # easy dependency management for nix projects
      nix-prefetch
      nix-prefetch-git
      nixfmt
    ] ++ lib.optionals stdenv.isDarwin [ cocoapods ]
    ++ lib.optionals stdenv.isLinux [ docker docker-compose ];
}
