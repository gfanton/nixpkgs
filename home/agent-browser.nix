{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  # A profile of the regular Chrome per identity. agent-browser resolves a name,
  # not a path, and runs every session on its own copy of that profile.
  templates = {
    work = "agent-work";
    perso = "agent-perso";
  };

  chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
in
lib.mkIf isDarwin {
  home.packages = [ pkgs.agent-browser ];

  home.sessionVariables.AGENT_BROWSER_SOCKET_DIR = "${config.xdg.stateHome}/agent-browser";

  xdg.configFile = lib.mapAttrs' (
    name: profile:
    lib.nameValuePair "agent-browser/${name}.json" {
      text = builtins.toJSON {
        executablePath = chrome;
        inherit profile;
        screenshotDir = "${config.xdg.cacheHome}/agent-browser/screenshots";
      };
    }
  ) templates;
}
