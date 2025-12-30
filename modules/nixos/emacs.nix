# NixOS Emacs daemon module
# Uses pkgs.myEmacs from overlay for single source of truth
{ config, lib, pkgs, ... }:

{
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.myEmacs;
  };

  # Ensure emacs-related fonts are available system-wide
  fonts.packages = with pkgs; [
    emacs-all-the-icons-fonts
  ];
}
