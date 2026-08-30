{ config, lib, ... }:

let
  inherit (lib) mkIf;
  mkIfCaskPresent = cask: mkIf (lib.any (x: x.name == cask) config.homebrew.casks);
  brewEnabled = config.homebrew.enable;
in
{
  environment.shellInit = mkIf brewEnabled ''
    eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
  '';

  documentation.enable = false;

  homebrew.enable = true;
  homebrew.onActivation.cleanup = "zap";
  homebrew.global.brewfile = true;

  # No taps: everything below comes from homebrew/core and homebrew/cask, which
  # are trusted implicitly. A non-official tap needs `trusted = true` or Homebrew
  # refuses to load from it (HOMEBREW_REQUIRE_TAP_TRUST, on by default since 6.0.0).
  homebrew.taps = [ ];

  # Prefer installing application from the Mac App Store
  #
  # Commented apps suffer continual update issue:
  # https://github.com/malob/nixpkgs/issues/9
  homebrew.masApps = {
    DaisyDisk = 411643860;
    Xcode = 497799835;
  };

  # If an app isn't available in the Mac App Store, or the version in the App Store has
  # limitiations, e.g., Transmit, install the Homebrew Cask.
  homebrew.casks = [
    "font-fira-mono-nerd-font"
    "font-fira-code-nerd-font"
    "font-jetbrains-mono-nerd-font"
    "font-sauce-code-pro-nerd-font"
    "orbstack"
    "arc"
    "raycast"
    "1password"
    "1password-cli"
    "whisky"
    "keycastr"
    "numi"
    "amethyst"
    "tailscale-app"
  ];

  # Configuration related to casks

  # setup 1password ssh agent
  # https://developer.1password.com/docs/ssh/get-started/#step-4-configure-your-ssh-or-git-client
  environment.variables.SSH_AUTH_SOCK = mkIfCaskPresent "1password-cli" "/Users/${config.users.primaryUser.username}/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";

  # Tailscale runs tailscaled inside the app sandbox with no /var/run socket, so
  # only the bundled binary can reach the daemon.
  environment.shellAliases.tailscale = mkIfCaskPresent "tailscale-app" "/Applications/Tailscale.app/Contents/MacOS/Tailscale";

  # For cli packages that aren't currently available for macOS in `nixpkgs`.Packages should be
  # installed in `../home/default.nix` whenever possible.
  homebrew.brews = [
    { name = "ical-buddy"; }

    # {
    #   name = "koekeishiya/formulae/yabai";
    # }
    # {
    #   name = "koekeishiya/formulae/skhd";
    # }
    # {
    #   name = "sketchybar";
    #   start_service = false;
    #   restart_service = "changed";
    # }
  ];
}
