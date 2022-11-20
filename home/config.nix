{ lib, pkgs, config, ... }:

{
  # btop
  home.file."/.config/btop" = {
    source = "${lib.cleanSource ../config/btop}";
    recursive = true;
  };

  #npmrc
  home.file.".npmrc" = with pkgs; {
    source = writeText "npmrc" ''
      prefix=${config.xdg.dataHome}/node_modules
    '';
  };

  # link aspell config
  home.file.".aspell.config" = with pkgs; {
    source = writeText "aspell.conf" ''
      dict-dir ""
      master en_US
      extra-dicts en en-computers.rws en-science.rws fr.rws
    '';
  };

}
