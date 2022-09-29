{ config, lib, pkgs, ... }:
let inherit (config.home) user-info homeDirectory;
in {
  xdg = {
    enable = true;
    configHome = "${homeDirectory}/.config";
    cacheHome = "${homeDirectory}/.cache";
    dataHome = "${homeDirectory}/.local/share";
  };

  home.sessionPath = [
    # local bin folder
    "${homeDirectory}/.local/bin"
    # npm bin folder
    "${config.xdg.dataHome}/node_modules/bin"
  ];

  home.sessionVariables.LC_ALL = "en_US.UTF-8";
  home.sessionVariables.TERMINFO_DIRS =
    "${config.home.profileDirectory}/share/terminfo";

  # EDITOR = "${pkgs.emacsGcc}/bin/emacsclient -nw";

  # path
  # PKG_CONFIG_PATH = "${profile_dir}/lib/pkgconfig";
  # TERMINFO_DIRS = "${profile_dir}/share/terminfo";

  # flags
  # CFLAGS = "-I${profile_dir}/include";
  # CPPFLAGS = "-I${profile_dir}/include";
}
