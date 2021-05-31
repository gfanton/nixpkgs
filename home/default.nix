{ config, pkgs, lib, ... }:

let
  home_dir = "${config.home.homeDirectory}";
  profile_dir = "${config.home.profileDirectory}";
  spacemacsd = "${lib.cleanSource ../config/spacemacs}";
in
{
  # Import config broken out into files
  imports = [
    ./kitty.nix
    ./shells.nix
  ];

  xdg = {
    enable = true;
    configHome = "${home_dir}/.config";
    cacheHome = "${home_dir}/.cache";
    dataHome = "${home_dir}/.local/share";
  };

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    # Some basics
    mosh # wrapper for `ssh` that better and not dropping connections
    htop # fancy version of `top`
    unrar # extract RAR archives
    exa # fancy version of `ls`
    # stable.bandwhich # display current network utilization by process

    # pkgs silicon
    silicon.asciinema # Terminal session recorder
    silicon.tmate # instant terminal sharing
    silicon.fd # fancy version of `find`
    silicon.most
    silicon.parallel # runs commands in parallel
    silicon.socat
    silicon.less
    silicon.tree # list contents of directories in a tree-like format.
    silicon.coreutils
    silicon.jq
    silicon.mage # A Make/Rake-like Build Tool Using Go
    silicon.ripgrep # better version of grep
    silicon.curl # transfer a URL
    silicon.wget # The non-interactive network downloader.
    silicon.nextdns # nextdns
    # stable
    stable.procs # fancy version of `ps`

    # aspell
    silicon.aspell # interactive spell checker
    silicon.aspellDicts.fr
    silicon.aspellDicts.en
    silicon.aspellDicts.en-computers
    silicon.aspellDicts.en-science

    # antibody
    silicon.antibody

    # ruby
    (silicon.ruby_2_7.withPackages (ps: [
      ps.ffi-compiler
    ]))

    # js
    silicon.nodejs-16_x
    silicon.yarn
    silicon.watchman

    # python
    (silicon.python3.withPackages (p: with p; [
      virtualenv
      pip
      mypy
      pylint
      yapf
      setuptools
      python-language-server
    ]))

    # go tools
    silicon.gopls
    silicon.delve
    silicon.golangci-lint
    silicon.go2nix
    silicon.vgo2nix
    # exclude bundle
    (silicon.gotools.overrideDerivation (oldAttrs: {
      excludedPackages = oldAttrs.excludedPackages + "\\|\\(bundle\\)";
    }))

    # Useful nix related tools
    cachix # adding/managing alternative binary caches hosted by Cachix
    lorri # improve `nix-shell` experience in combination with `direnv`
    niv # easy dependency management for nix projects

    # Platform specific tools
 ] ++ lib.optionals stdenv.isDarwin [
    silicon.libffi
    silicon.libffi.dev
    silicon.cocoapods
    silicon.jazzy
    (silicon.gomobile.override {
      androidPkgs = pkgs.androidenv.composeAndroidPackages {
        includeNDK = true;
        ndkVersion = "21.3.6528147"; # WARNING: 22.0.7026061 is broken.
      };
      buildGoModule = silicon.buildGoModule;
      xcodeWrapperArgs = { version = "12.5"; };
    })
  ] ++ lib.optionals stdenv.isLinux [
    docker
    docker-compose
  ];

  # Go Env
  programs.go = {
    enable = true;
    goPath = "go";
    goBin = ".local/bin";
    package = pkgs.silicon.go_1_16;
  };

  # adnroid
  android-sdk = {
    enable = true;
    packages = sdk: with sdk; [
      build-tools-30-0-2
      cmdline-tools-latest
      emulator
      platforms-android-30
      sources-android-30
    ];
  };

  # Additional Path
  home.sessionPath = [
    "${home_dir}/.local/bin"
  ];

  # Additional env
  home.sessionVariables = {
    EDITOR = "${pkgs.emacsGcc}/bin/emacsclient -nw";

    # path
    PKG_CONFIG_PATH = "${profile_dir}/lib/pkgconfig";
    TERMINFO_DIRS = "${profile_dir}/share/terminfo";

    # flags
    # LDFLAGS="-L${profile_dir}/lib";
    CFLAGS="-I${profile_dir}/include";
    CPPFLAGS="-I${profile_dir}/include";
  };

  # lang
  home.language = {
    base = "en_US.UTF-8";
    address = "en_US.UTF-8";
    collate = "en_US.UTF-8";
    ctype = "en_US.UTF-8";
    measurement = "en_US.UTF-8";
    messages = "en_US.UTF-8";
    monetary = "en_US.UTF-8";
    name = "en_US.UTF-8";
    numeric = "en_US.UTF-8";
    paper = "en_US.UTF-8";
    telephone = "en_US.UTF-8";
    time = "en_US.UTF-8";
  };

  # manual
  manual.manpages.enable = true;

  programs.truecolor = {
    enable = true;
    useterm = "xterm-kitty";
    terminfo = "${pkgs.kitty.terminfo}/share/terminfo";
  };

  # Bat, a substitute for cat.
  # https://github.com/sharkdp/bat
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.bat.enable
  programs.bat = {
    enable = true;
    config = {
      style = "plain";
    };
  };

  # Direnv, load and unload environment variables depending on the current directory.
  # https://direnv.net
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.direnv.enable
  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };

  # Htop
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.htop.enable
  programs.htop.enable = true;

  # Zoxide, a faster way to navigate the filesystem
  # https://github.com/ajeetdsouza/zoxide
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.zoxide.enable
  programs.zoxide.enable = true;

  # emacs
  home.file.".emacs.d" = {
   source = pkgs.spacemacs;
   recursive = true;
  };

  home.file.".spacemacs.d" = {
   source = spacemacsd;
   recursive = true;
  };

  programs.emacs = {
    enable = true;
    # package = pkgs.silicon.emacs;
    package = pkgs.emacsGcc;
  };

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

  programs.gpg = {
    homedir = "${config.xdg.dataHome}/gnupg";
    enable = true;
  };

  # link aspell config
  home.file.".aspell.config" = with pkgs; {
    source = writeText "aspell.conf" ''
    master en_US
    extra-dicts en-computers.rws en-science.rws fr.rws
    '';
  };

  # android-sdk = {
  #   enable = true;

  #   # Optional; default path is "~/.local/share/android".
  #   path = "${config.home.homeDirectory}/.local/android/sdk";
  #   packages = sdk: with sdk; [
  #     build-tools-30-0-2
  #     cmdline-tools-latest
  #     emulator
  #     platforms-android-30
  #     sources-android-30
  #   ];
  # };

  # Git
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.git.enable
  # Aliases config imported in flake.
  programs.git ={
    enable = true;
    userEmail = "8671905+gfanton@users.noreply.github.com";
    userName = "gfanton";
    aliases = {
      d = "diff";
      lg = "log --graph --abbrev-commit --decorate --format=format:'%C(blue)%h%C(reset) - %C(green)(%ar)%C(reset) %s %C(italic)- %an%C(reset)%C(magenta bold)%d%C(reset)' --all";
      co = "checkout";
    };
    package = pkgs.buildEnv {
      name = "myGitEnv";
      paths = with pkgs.silicon.gitAndTools; [git gh tig];
    };
    delta.enable = true;
    lfs.enable = true;
    ignores = [
      "*~"
      "*.swp"
      "*#"
      ".#*"
      ".DS_Store"
    ];
    extraConfig = {
      github.user = "gfanton";
      core = {
        whitespace = "trailing-space,space-before-tab";
        editor = "em";
      };
      pull.rebase = true;
      url."git@github.com:".insteadOf = "https://github.com/";
    };
  };

  # This value determines the Home Manager release that your configuration is compatible with. This
  # helps avoid breakage when a new Home Manager release introduces backwards incompatible changes.
  #
  # You can update Home Manager without changing this value. See the Home Manager release notes for
  # a list of state version changes in each release.
  home.stateVersion = "20.09";
}
