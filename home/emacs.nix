{ config, lib, pkgs, ... }:

let
  inherit (config.home) user-info homeDirectory;
  xterm-emacsclient = pkgs.writeShellScriptBin "xemacsclient" ''
    export TERM=xterm-emacs
    ${pkgs.emacsGcc}/bin/emacsclient $@
  '';
  xterm-emacs = pkgs.writeShellScriptBin "xemacs" ''
    export TERM=xterm-emacs
    ${pkgs.emacsGcc}/bin/emacs $@
  '';
in {

  ## home dot D
  # spacemacs
  home.file."emacs/spacemacs" = {
    source = pkgs.spacemacs;
    recursive = true;
  };

  # home.file."spacemacs" = {
  #  source = pkgs.spacemacs;
  #  recursive = true;
  # };

  # home.file."emacs/doom" = {
  #  source = pkgs.doomemacs;
  #  recursive = true;
  # };

  home.file.".emacs.d" = {
    source = pkgs.chemacs2;
    recursive = true;
  };

  home.file.".emacs-profiles.el" = with pkgs; {
    source = writeText "emacs-profiles" ''
      (
       ("doom" . ((user-emacs-directory . "~/emacs/doomemacs")
             (env . (("DOOMDIR" . "~/doom-config")))))
       ("spacemacs" . ((user-emacs-directory . "~/emacs/spacemacs")))
      )
    '';
  };

  home.file.".emacs-profile" = with pkgs; {
    source = writeText "emacs-profiles" "doom";
  };

  # spacemacs
  home.file.".spacemacs.d" = {
    source = "${lib.cleanSource ../config/spacemacs}";
    recursive = true;
  };

  programs.emacs.enable = true;
  programs.emacs.package = pkgs.emacsGcc;

  # setup alias
  programs.zsh.shellAliases.emacs = "${xterm-emacs}/bin/xemacs";
  programs.zsh.shellAliases.emacsclient =
    "${xterm-emacsclient}/bin/xemacsclient";
  programs.zsh.shellAliases.ec = "${xterm-emacsclient}/bin/xemacsclient -nw";
}
