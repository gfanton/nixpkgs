{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.home) user-info homeDirectory;
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
  # Import the new Nix-native Emacs configuration
  imports = [
    ./emacs-nix.nix
  ];

  # Export xterm-emacsclient for use in other modules
  _module.args.xemacsclient = xterm-emacsclient;

  ## home dot D
  # spacemacs
  home.file."emacs/spacemacs" = {
    source = pkgs.spacemacs;
    recursive = true;
  };

  # home.file."emacs/doom" = {
  #  source = pkgs.doomemacs;
  #  recursive = true;
  # };

  home.file.".emacs.d" = {
    source = pkgs.chemacs2;
    recursive = true;
  };

  home.file.".emacs-profile" = with pkgs; {
    source = writeText "emacs-profiles" "nix-vanilla";
  };
  # Note: .emacs-profiles.el is now managed by emacs-nix.nix to include all profiles

  home.packages = [ pkgs.sqlite ]; # add sqlite packages required by magit

  # spacemacs
  home.file.".spacemacs.d" = {
    source = "${lib.cleanSource ../config/spacemacs}";
    recursive = true;
  };

  # Emacs program configuration will be managed by individual profiles
  # programs.emacs.enable = true;
  # programs.emacs.package = pkgs.emacs-gtk;

  # Spacemacs-specific aliases (explicit terminal-only commands)
  programs.zsh.shellAliases = {
    # Explicit spacemacs commands (terminal-only) - main emacs/emacsclient handled by emacs-nix.nix
    "spacemacs" = "${xterm-emacs}/bin/xemacs -nw --with-profile=spacemacs";
    "spacemacsclient" = "${xterm-emacsclient}/bin/xemacsclient -nw";
  };
}
