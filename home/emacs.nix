# Home-manager emacs configuration
# Uses pkgs.myEmacs from overlay (single source of truth)
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.home) user-info homeDirectory;

  # Use emacs from overlay (defined in overlays/emacs.nix)
  myEmacs = pkgs.myEmacs;

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

  # Ensure log directory exists for emacs-nix logging
  home.file.".local/log/.keep".text = "";

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

  # Emacs and dev packages from overlay
  home.packages = pkgs.myEmacsDevPackages ++ [ myEmacs ];

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
