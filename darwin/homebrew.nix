{ config, lib, ... }:

let
  inherit (lib) mkIf;
  mkIfCaskPresent = cask:
    mkIf (lib.any (x: x.name == cask) config.homebrew.casks);
  brewEnabled = config.homebrew.enable;

in {
  environment.shellInit = mkIf brewEnabled ''
    eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
  '';

  documentation.enable = false;

  homebrew.enable = true;
  homebrew.onActivation.cleanup = "zap";
  homebrew.global.brewfile = true;

  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-drivers"
    "homebrew/cask-fonts"
    "homebrew/cask-versions"
    "homebrew/core"
    "homebrew/services"
    "nrlquaker/createzap"
    "koekeishiya/formulae"
    "FelixKratz/formulae"
  ];

  # Prefer installing application from the Mac App Store
  #
  # Commented apps suffer continual update issue:
  # https://github.com/malob/nixpkgs/issues/9
  homebrew.masApps = {
    DaisyDisk = 411643860;
    Numbers = 409203825;
    Pages = 409201541;
    # "1Blocker" = 1365531024;
    # "1Password for Safari" = 1569813296;
    # "1Password" = 1333542190;
    # "Accelerate for Safari" = 1459809092;
    # "Apple Configurator 2" = 1037126344;
    # "Dark Mode for Safari" = 1397180934;
    # "Gemini 2" = 1090488118;
    # "Pixelmator Classic" = 407963104;
    # "Pixelmator Pro" = 1289583905;
    # "Save to Raindrop.io" = 1549370672;
    # "Things 3" = 904280696;
    # "WiFi Explorer" = 494803304;
    # "Yubico Authenticator" = 1497506650;
    # "iMazing Profile Editor" = 1487860882;
    # Deliveries = 290986013;
    # Fantastical = 975937182;
    # Keynote = 409183694;
    # MindNode = 1289197285;
    # Patterns = 429449079;
    # SiteSucker = 442168834;
    # Slack = 803453959;
    # TODO: Re-enable once macOS Ventura officially launches
    # TripMode = 1513400665;
    # Ulysses = 1225570693;
    # Vimari = 1480933944;
    # Xcode = 497799835;
  };

  # If an app isn't available in the Mac App Store, or the version in the App Store has
  # limitiations, e.g., Transmit, install the Homebrew Cask.
  homebrew.casks = [
    "font-hack-nerd-font"
    "orbstack"
    # "1password"
    # "1password-cli"
    # "amethyst"
    # "anki"
    # "arq"
    # "balenaetcher"
    # "cleanmymac"
    # "element"
    # "etrecheckpro"
    # "discord"
    # "firefox"
    # "google-chrome"
    # "google-drive"
    # "gpg-suite"
    # "hammerspoon"
    # "keybase"
    # "nvidia-geforce-now"
    # "obsbot-me-tool"
    # "obsbot-tinycam"
    # "obsidian"
    # "parallels"
    # "postman"
    # "protonvpn"
    # "raindropio"
    # "raycast"
    # "secretive"
    # "signal"
    # "skype"
    # "steam"
    # "superhuman"
    # "tor-browser"
    # "transmission"
    # "transmit"
    # "visual-studio-code"
    # "vlc"
    # "yubico-yubikey-manager"
    # "yubico-yubikey-personalization-gui"
  ];

  # Configuration related to casks
  # environment.variables.SSH_AUTH_SOCK = mkIfCaskPresent "1password-cli"
  #   "/Users/${config.users.primaryUser.username}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";

  # For cli packages that aren't currently available for macOS in `nixpkgs`.Packages should be
  # installed in `../home/default.nix` whenever possible.
  homebrew.brews = [
    { name = "ical-buddy"; }
    {
      name = "koekeishiya/formulae/yabai";
      start_service = true;
      restart_service = "changed";
    }
    {
      name = "koekeishiya/formulae/skhd";
      start_service = true;
      restart_service = "changed";
    }
    {
      name = "sketchybar";
      start_service = true;
      restart_service = "changed";
    }
  ];
}
