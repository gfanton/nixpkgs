{ config, pkgs, lib, ... }:

 let
  spacemacsd = "${lib.cleanSource ../config/spacemacs}";
in
{
  # Import config broken out into files
  imports = [
    ./git.nix
    ./kitty.nix
    ./shells.nix
  ];

  home.packages = with pkgs; [
    # Some basics
    mosh # wrapper for `ssh` that better and not dropping connections
    htop # fancy version of `top`
    unrar # extract RAR archives
    exa # fancy version of `ls`

    # pkgs silicon
    silicon.tmate # instant terminal sharing
    silicon.fd # fancy version of `find`
    silicon.bandwhich # display current network utilization by process
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

    # stable
    stable.procs # fancy version of `ps`

    # nextdns
    silicon.nextdns

    # aspell
    silicon.aspell # interactive spell checker
    silicon.aspellDicts.fr
    silicon.aspellDicts.en
    silicon.aspellDicts.en-computers
    silicon.aspellDicts.en-science

    # antibody
    silicon.antibody

    # ruby
    (ruby_2_7.withPackages (ps: [
      ps.ffi-compiler
    ]))

    # js
    silicon.nodejs
    silicon.yarn

    # python
    (python3.withPackages (p: with p; [
      virtualenv
      pip
      mypy
      pylint
      yapf
      setuptools
      python-language-server
    ]))

    # Useful nix related tools
    cachix # adding/managing alternative binary caches hosted by Cachix
    lorri # improve `nix-shell` experience in combination with `direnv`
    niv # easy dependency management for nix projects

  ] ++ lib.optionals stdenv.isDarwin [
    libffi
    libffi.dev
    cocoapods
    jazzy
  ];


  # Additional Path
  home.sessionPath = [
    "$HOME/.local/bin"
  ];

  # Additional env
  home.sessionVariables = {
    # path
    LIBRARY_PATH="$HOME/.nix-profile/lib";
    LD_LIBRARY_PATH="$HOME/.nix-profile/lib";
    CPATH="$HOME/.nix-profile/include";
    PKG_CONFIG_PATH = "$HOME/.nix-profile/lib/pkgconfig";

    # flags
    # LDFLAGS="-L$HOME/.nix-profile/lib";
    CFLAGS="-I$HOME/.nix-profile/include";
    CPPFLAGS="-I$HOME/.nix-profile/include";
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
    config= {
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
  programs.htop = {
    enable = true;
    hideKernelThreads = true;
    hideThreads = true;
    hideUserlandThreads = true;
    treeView = true;
    meters = {
      left = ["LeftCPUs2" "Memory" "Swap" "Load" "Clock"];
      right = ["RightCPUs2" "Tasks" "LoadAverage" "Uptime"];
    };
  };

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
    package = pkgs.emacs;
  };

  # link aspell config
  home.file.".aspell.config" = with pkgs; {
    source = writeText "aspell.conf" ''
    master en_US
    extra-dicts en-computers.rws en-science.rws fr.rws
    '';
  };

  # Go Env
  programs.go = {
    enable = true;
    goPath = "go";
    goBin = ".local/bin";
    package = (pkgs.buildEnv {
      name = "golang";
      paths = with pkgs; [
        # package = pkgs.go_1_15;
        silicon.go_1_16
        silicon.gopls
        silicon.delve
        silicon.golangci-lint
        silicon.go2nix
        silicon.vgo2nix
        # exclude bundle
        (silicon.gotools.overrideDerivation (oldAttrs: {
          excludedPackages = oldAttrs.excludedPackages + "\\|\\(bundle\\)";
        }))
      ];
    });
  };

  # This value determines the Home Manager release that your configuration is compatible with. This
  # helps avoid breakage when a new Home Manager release introduces backwards incompatible changes.
  #
  # You can update Home Manager without changing this value. See the Home Manager release notes for
  # a list of state version changes in each release.
  home.stateVersion = "21.03";
}
