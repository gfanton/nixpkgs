{ config, ... }:

{
  system.activationScripts.extraUserActivation.text =
    config.system.activationScripts.brew-bundle.text;

  programs.brew-bundle.enable = true;
  programs.brew-bundle.autoUpdate = true;
  programs.brew-bundle.cleanup = "zap";

  programs.brew-bundle.taps = [
    "homebrew/cask"
    "homebrew/cask-drivers"
    "homebrew/cask-fonts"
    "homebrew/cask-versions"
    "homebrew/core"
    "homebrew/services"
  ];

  programs.brew-bundle.casks = [
    "atom"
    "amethyst"
    "arq"
    "balenaetcher"
    "firefox"
    "google-chrome"
    "google-drive-file-stream"
    "gpg-suite"
    "hammerspoon"
    "hey"
    "honer"
    "keybase"
    "launchbar"
    "linear-linear"
    "protonvpn"
    "signal"
    "skype"
    "steam"
    "superhuman"
    "tor-browser"
    "transmission"
    "transmit"
    "typinator"
    "virtualbox"
    "virtualbox-extension-pack"
    "visual-studio-code-insiders"
    "vlc"
    "yubico-authenticator"
    "yubico-yubikey-manager"
    "yubico-yubikey-personalization-gui"
  ];

  programs.brew-bundle.masApps = {
    "1Blocker" = 1107421413;
    "1Password" = 1333542190;
    DaisyDisk = 411643860;
    "Dark Mode for Safari" = 1397180934;
    Deliveries = 924726344;
    Fantastical = 975937182;
    Keynote = 409183694;
    "LG Screen Manager" = 1142051783;
    MindNode = 1289197285;
    Numbers = 409203825;
    Pages = 409201541;
    Patterns = 429449079;
    Pixelmator = 407963104;
    "Pixelmator Pro" = 1289583905;
    Slack = 803453959;
    SiteSucker = 442168834;
    "Things 3" = 904280696;
    "Type Fu" = 509818877;
    Ulysses = 1225570693;
    "WiFi Explorer" = 494803304;
    Xcode = 497799835;
  };
}