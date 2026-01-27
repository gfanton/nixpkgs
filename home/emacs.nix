# Home-manager emacs configuration
# Uses pkgs.myEmacs from overlay (single source of truth)
{
  pkgs,
  ...
}:
let
  # Use emacs from overlay (defined in overlays/emacs.nix)
  myEmacs = pkgs.myEmacs;

  # Terminal-compatible emacsclient wrapper (used by other modules)
  xterm-emacsclient = pkgs.writeShellScriptBin "xemacsclient" ''
    export TERM=xterm-emacs
    ${myEmacs}/bin/emacsclient "$@"
  '';
in
{
  # Ensure log directory exists for emacs logging
  home.file.".local/log/.keep".text = "";

  # Install nix-vanilla as primary .emacs.d
  home.file.".emacs.d/init.el" = {
    source = ../config/emacs/nix-vanilla/init.el;
  };

  home.file.".emacs.d/config.org" = {
    source = ../config/emacs/nix-vanilla/config.org;
  };

  # === PACKAGES ===

  # Emacs and dev packages from overlay
  home.packages = pkgs.myEmacsDevPackages ++ [ myEmacs ];

  # === SHELL ALIASES ===

  programs.zsh.shellAliases = {
    "emacs" = "${myEmacs}/bin/emacs -nw";
    "emacsclient" = "${myEmacs}/bin/emacsclient -nw";
  };

  # === MODULE EXPORTS ===

  # Export modern Emacs for use in other modules
  _module.args.myEmacs = myEmacs;
  # Export xterm-compatible wrapper for kitty/alacritty/etc.
  _module.args.xemacsclient = xterm-emacsclient;
}
