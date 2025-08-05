{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.home) user-info homeDirectory;

  # Emacs from overlay with optimizations (terminal)
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
      corfu-terminal  # Terminal support for Emacs 30
      popon           # Required dependency for corfu-terminal
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

      # Custom modes from your Spacemacs config
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

  # Custom packages needed (from your Spacemacs config)
  customPackages = with pkgs; [
    # LSP servers and tools
    gopls
    # rust-analyzer conflicts with rustup, will be provided by system
    nodePackages.typescript-language-server
    nodePackages.eslint
    nixd
    nil # Alternative Nix LSP server

    # Formatters
    gofumpt
    # rustfmt conflicts with rustup, will be provided by system
    nodePackages.prettier

    # Other tools
    ripgrep
    fd
    sqlite
    git
    # aspell is already provided by the system, avoid conflicts
    # aspell
    # aspellDicts.en
    # aspellDicts.en-computers
    # aspellDicts.en-science

    # Fonts
    jetbrains-mono
    nerd-fonts.jetbrains-mono

    # Tree-sitter grammars will be managed by Emacs packages instead
  ];

  # Clean terminal wrappers for nix-vanilla (no xterm overrides)
  # Note: xterm overrides are preserved for Spacemacs in emacs.nix

in
{
  # Install the new Emacs configuration using separate files
  home.file."emacs/nix-vanilla/init.el" = {
    source = ../config/emacs/nix-vanilla/init.el;
  };

  home.file."emacs/nix-vanilla/config.org" = {
    source = ../config/emacs/nix-vanilla/config.org;
  };

  # Add the new profile to chemacs profiles
  home.file.".emacs-profiles.el" = with pkgs; {
    source = writeText "emacs-profiles" ''
      (
       ("doom" . ((user-emacs-directory . "~/emacs/doomemacs")
             (env . (("DOOMDIR" . "~/doom-config")))))
       ("spacemacs" . ((user-emacs-directory . "~/emacs/spacemacs")))
       ("nix-vanilla" . ((user-emacs-directory . "~/emacs/nix-vanilla")))
      )
    '';
  };

  # Install additional packages needed for the configuration
  home.packages = customPackages ++ [
    # Make myEmacs available through aliases
    myEmacs
  ];

  # Export the new emacs for use in other modules
  _module.args.myEmacs = myEmacs;

  # Shell aliases - nix-vanilla is default, all terminal-only
  programs.zsh.shellAliases = {
    # Default emacs commands use nix-vanilla (terminal-only)
    "emacs" = "${myEmacs}/bin/emacs -nw --with-profile=nix-vanilla";
    "emacsclient" = "${myEmacs}/bin/emacsclient -nw";

    # Explicit nix-vanilla aliases (for clarity/backward compatibility)
    "emacs-nix" = "${myEmacs}/bin/emacs -nw --with-profile=nix-vanilla";
    "emacsclient-nix" = "${myEmacs}/bin/emacsclient -nw";
    "emacs-nix-nw" = "${myEmacs}/bin/emacs -nw --with-profile=nix-vanilla";
    "emacsclient-nix-nw" = "${myEmacs}/bin/emacsclient -nw";
  };

  # Tree-sitter will be managed by Emacs tree-sitter packages
}
