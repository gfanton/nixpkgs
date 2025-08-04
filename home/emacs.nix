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
    ${pkgs.emacs-gtk}/bin/emacsclient $@
  '';
  xterm-emacs = pkgs.writeShellScriptBin "xemacs" ''
    export TERM=xterm-emacs
    ${pkgs.emacs-gtk}/bin/emacs $@
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
    source = writeText "emacs-profiles" "spacemacs";
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

  # setup alias
  programs.zsh.shellAliases.xemacs = "${xterm-emacs}/bin/xemacs --with-profile=spacemacs";
  programs.zsh.shellAliases.xemacsclient = "${xterm-emacsclient}/bin/xemacsclient";
  programs.zsh.shellAliases.emacs = "xemacs -nw";
  programs.zsh.shellAliases.emacsclient = "xemacsclient -nw";
}
