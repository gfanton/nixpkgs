# Emacs overlay - Single source of truth for emacs package
# Used by: home-manager, NixOS modules, Darwin modules
final: prev:
let
  # Modern Emacs with optimizations
  emacs-base = prev.emacs-pgtk.override {
    withNativeCompilation = true;
    withTreeSitter = true;
    withSQLite3 = true;
  };

  # Package overrides for compatibility
  packageOverrides = self: super: {
    org = super.elpaPackages.org;
  };

  # Build emacs packages set with overrides
  emacsPackagesFor = (prev.emacsPackagesFor emacs-base).overrideScope packageOverrides;

  # Comprehensive package list
  emacsPackages =
    epkgs:
    with epkgs;
    let
      # Custom local packages
      gno-mode = trivialBuild {
        pname = "gno-mode";
        version = "0.1";
        src = ../config/emacs/packages/gno;
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
        src = ../config/emacs/packages/gotemplate;
        packageRequires = [ ];
      };

      magit-live-diff = trivialBuild {
        pname = "magit-live-diff";
        version = "1.1.0";
        src = ../config/emacs/packages/magit-live-diff;
        packageRequires = [ magit ];
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

      # Completion UI (Ultimate Corfu setup)
      corfu
      corfu-terminal
      popon
      cape
      kind-icon
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
      consult-lsp

      # Programming languages
      go-mode
      rust-mode
      typescript-mode
      js2-mode
      rjsx-mode
      web-mode
      nix-mode
      yaml-mode
      json-mode
      markdown-mode

      # Git integration
      magit
      magit-section
      git-timemachine
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
      clipetty

      # Utilities
      expand-region
      multiple-cursors
      avy
      smartparens
      rainbow-delimiters
      editorconfig
      flycheck
      yasnippet
      yasnippet-snippets
      vundo

      # Custom modes
      polymode
      templ-ts-mode

      # Tree-sitter grammars
      (treesit-grammars.with-grammars (
        grammars: with grammars; [
          tree-sitter-go
          tree-sitter-gomod
          tree-sitter-rust
          tree-sitter-typescript
          tree-sitter-tsx
          tree-sitter-javascript
          tree-sitter-json
          tree-sitter-yaml
          tree-sitter-toml
          tree-sitter-nix
          tree-sitter-markdown
          tree-sitter-html
          tree-sitter-css
          tree-sitter-bash
          tree-sitter-python
          tree-sitter-c
          tree-sitter-cpp
          tree-sitter-dockerfile
          tree-sitter-make
          tree-sitter-lua
        ]
      ))

      # Local custom packages
      gno-mode
      go-template-mode
      magit-live-diff
    ];
in
{
  # Single emacs package with all packages included
  myEmacs = emacsPackagesFor.emacsWithPackages emacsPackages;

  # Also export dev packages for convenience
  myEmacsDevPackages = with prev; [
    gopls
    nodePackages.typescript-language-server
    nodePackages.eslint
    nixd
    nil
    emacs-lsp-booster
    gofumpt
    nodePackages.prettier
    ripgrep
    fd
    sqlite
    git
    jetbrains-mono
    nerd-fonts.jetbrains-mono
  ];
}
