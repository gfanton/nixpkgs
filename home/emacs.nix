{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.home) user-info homeDirectory;

  # Modern Emacs with optimizations (primary configuration)
  emacs-base = pkgs.emacs-pgtk.override {
    withNativeCompilation = true;
    withTreeSitter = true;
    withSQLite3 = true;
  };

  # Comprehensive package list following 2024-2025 best practices
  emacsPackages =
    epkgs:
    with epkgs;
    let
      # Custom local packages
      gno-mode = trivialBuild {
        pname = "gno-mode";
        version = "0.1";
        src = ../config/spacemacs/packages/gno;
        packageRequires = [
          go-mode
          lsp-mode
          flycheck
          polymode
        ];
      };

      go-template-mode = trivialBuild {
        pname = "go-template-mode";
        version = "1.0";
        src = ../config/spacemacs/packages/gotemplate;
        packageRequires = [ ];
      };
    in
    [
      # Core framework
      use-package
      diminish
      bind-key

      # Modern completion framework (Vertico ecosystem)
      vertico
      consult
      embark
      embark-consult
      marginalia
      orderless

      # Completion UI
      corfu
      corfu-terminal # Terminal support for Emacs 30
      popon # Required dependency for corfu-terminal
      cape
      wgrep

      # Evil mode + vim bindings
      evil
      evil-collection
      evil-surround
      evil-numbers
      evil-org
      general
      which-key

      # LSP integration
      lsp-mode
      lsp-ui
      consult-lsp

      # Programming languages
      # Go
      go-mode
      # Rust
      rust-mode
      # TypeScript/JavaScript
      typescript-mode
      js2-mode
      rjsx-mode
      # Web
      web-mode
      # Nix
      nix-mode
      # YAML/JSON
      yaml-mode
      json-mode
      # Markdown
      markdown-mode

      # Git integration
      magit
      magit-section
      forge
      git-link
      diff-hl

      # Project management
      projectile
      consult-projectile

      # Org mode
      org
      org-cliplink

      # UI enhancements
      catppuccin-theme
      doom-modeline
      all-the-icons

      # Terminal integration
      vterm
      clipetty  # OSC 52 clipboard support for universal clipboard integration

      # Utilities
      expand-region
      multiple-cursors
      avy
      smartparens
      rainbow-delimiters
      ws-butler
      editorconfig
      flycheck
      yasnippet
      yasnippet-snippets

      # Custom modes
      polymode
      templ-ts-mode

      # Tree-sitter grammars (from emacs-overlay)
      treesit-grammars.with-all-grammars

      # Local custom packages
      gno-mode
      go-template-mode
    ];

  # Package overrides for compatibility
  packageOverrides = self: super: {
    # Ensure we use the right org version
    org = super.elpaPackages.org;
  };

  # Build final Emacs with all packages
  myEmacs = ((pkgs.emacsPackagesFor emacs-base).overrideScope packageOverrides).emacsWithPackages emacsPackages;

  # Development tools and LSP servers
  devPackages = with pkgs; [
    # LSP servers and tools
    gopls
    nodePackages.typescript-language-server
    nodePackages.eslint
    nixd
    nil # Alternative Nix LSP server

    # Formatters
    gofumpt
    nodePackages.prettier

    # Essential tools
    ripgrep
    fd
    sqlite
    git

    # Fonts
    jetbrains-mono
    nerd-fonts.jetbrains-mono
  ];

  # Legacy xterm wrappers for Spacemacs compatibility
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
  # === PRIMARY CONFIGURATION: Nix-Vanilla (Modern Terminal-First Emacs) ===

  # Install nix-vanilla configuration files
  home.file."emacs/nix-vanilla/init.el" = {
    source = ../config/emacs/nix-vanilla/init.el;
  };

  home.file."emacs/nix-vanilla/config.org" = {
    source = ../config/emacs/nix-vanilla/config.org;
  };

  # === LEGACY CONFIGURATIONS ===

  # Spacemacs
  home.file."emacs/spacemacs" = {
    source = pkgs.spacemacs;
    recursive = true;
  };

  home.file.".spacemacs.d" = {
    source = "${lib.cleanSource ../config/spacemacs}";
    recursive = true;
  };

  # === CHEMACS2 PROFILE MANAGEMENT ===

  # Chemacs2 for profile switching
  home.file.".emacs.d" = {
    source = pkgs.chemacs2;
    recursive = true;
  };

  # Default profile: nix-vanilla (modern terminal-first)
  home.file.".emacs-profile" = with pkgs; {
    source = writeText "emacs-profile" "nix-vanilla";
  };

  # Profile definitions
  home.file.".emacs-profiles.el" = with pkgs; {
    source = writeText "emacs-profiles" ''
      (
       ("nix-vanilla" . ((user-emacs-directory . "~/emacs/nix-vanilla")))
       ("spacemacs" . ((user-emacs-directory . "~/emacs/spacemacs")))
       ("doom" . ((user-emacs-directory . "~/emacs/doomemacs")
             (env . (("DOOMDIR" . "~/doom-config")))))
      )
    '';
  };

  # === PACKAGES ===

  home.packages = devPackages ++ [ myEmacs ];

  # === SHELL ALIASES ===

  programs.zsh.shellAliases = {
    # Primary commands use nix-vanilla (modern terminal-first configuration)
    "emacs" = "${myEmacs}/bin/emacs -nw --with-profile=nix-vanilla";
    "emacsclient" = "${myEmacs}/bin/emacsclient -nw";

    # Explicit nix-vanilla aliases
    "emacs-nix" = "${myEmacs}/bin/emacs -nw --with-profile=nix-vanilla";
    "emacsclient-nix" = "${myEmacs}/bin/emacsclient -nw";

    # Legacy Spacemacs aliases (terminal-only with xterm compatibility)
    "spacemacs" = "${xterm-emacs}/bin/xemacs -nw --with-profile=spacemacs";
    "spacemacsclient" = "${xterm-emacsclient}/bin/xemacsclient -nw";
  };

  # === MODULE EXPORTS ===

  # Export modern Emacs for use in other modules
  _module.args.myEmacs = myEmacs;
  # Export legacy xterm wrapper for compatibility
  _module.args.xemacsclient = xterm-emacsclient;
}
