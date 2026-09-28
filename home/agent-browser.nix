{
  config,
  lib,
  pkgs,
  ...
}:

let
  # CDP port per Claude identity; the Claude shims pick the identity.
  identities = {
    work = 9222;
    perso = 9223;
  };

  chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
  profileDir = name: "${config.home.homeDirectory}/Library/Application Support/agent-chrome/${name}";
in
lib.mkIf pkgs.stdenv.isDarwin {
  home.packages = [ pkgs.agent-browser ];

  launchd.agents = lib.mapAttrs' (
    name: port:
    lib.nameValuePair "agent-chrome-${name}" {
      enable = true;
      config = {
        ProgramArguments = [
          chrome
          "--user-data-dir=${profileDir name}"
          "--remote-debugging-port=${toString port}"
          "--no-first-run"
          "--no-default-browser-check"
        ];
        RunAtLoad = true;
        # A deliberate quit exits 0 and stays quit; a crash restarts.
        KeepAlive.SuccessfulExit = false;
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/agent-chrome-${name}.log";
      };
    }
  ) identities;

  xdg.configFile = lib.mapAttrs' (
    name: port:
    lib.nameValuePair "agent-browser/${name}.json" {
      text = builtins.toJSON {
        cdp = toString port;
        pinTab = true;
      };
    }
  ) identities;
}
