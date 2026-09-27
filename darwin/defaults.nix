{ config, lib, ... }:

{
  system.defaults.NSGlobalDomain = {
    "com.apple.trackpad.scaling" = 3.0;
    # macOS has no Light value, it drops the key instead, so nix can only
    # assert Dark: a manual switch to Light holds until the next rebuild.
    AppleInterfaceStyle = "Dark";
    AppleInterfaceStyleSwitchesAutomatically = false;
    AppleMeasurementUnits = "Centimeters";
    AppleMetricUnits = 1;
    AppleShowScrollBars = "Automatic";
    AppleTemperatureUnit = "Celsius";
    # Disable automatic macOS window→tab merging so yabai tiles each window
    # predictably. Apps can still create tabs explicitly (e.g. Ghostty new_tab).
    AppleWindowTabbingMode = "manual";
    InitialKeyRepeat = 15;
    KeyRepeat = 1;
    NSAutomaticCapitalizationEnabled = false;
    NSAutomaticDashSubstitutionEnabled = false;
    NSAutomaticPeriodSubstitutionEnabled = false;
    NSWindowResizeTime = 1.0e-2;
    _HIHideMenuBar = false;
  };

  # Firewall
  networking.applicationFirewall = {
    enable = true;
    blockAllIncoming = false;
    allowSigned = true;
    allowSignedApp = true;
    enableStealthMode = true;
  };

  # Dock and Mission Control
  system.defaults.dock = {
    autohide = true;
    expose-group-apps = false;
    mru-spaces = false;
    tilesize = 25;
    # Disable all hot corners
    wvous-bl-corner = 1;
    wvous-br-corner = 1;
    wvous-tl-corner = 1;
    wvous-tr-corner = 1;
    # disable animation
    launchanim = false;
    autohide-delay = 0.1;
    autohide-time-modifier = 0.1;
    expose-animation-duration = 0.1;
  };

  # Login and lock screen
  system.defaults.loginwindow = {
    GuestEnabled = false;
    DisableConsoleAccess = true;
  };

  # Spaces
  system.defaults.spaces.spans-displays = false;

  # Trackpad
  system.defaults.trackpad = {
    Clicking = true;
    TrackpadRightClick = true;
  };

  # Finder
  system.defaults.finder = {
    ShowStatusBar = true;
    AppleShowAllFiles = true;
    FXEnableExtensionChangeWarning = true;
    AppleShowAllExtensions = true;
    QuitMenuItem = true;
  };

  # The dictionary is written whole and replaces the stored one, so an ID left
  # out falls back to its macOS default. A shortcut is a character, a keycode
  # and a modifier mask, 65535 meaning none. Modifier bits: shift 131072, ctrl
  # 262144, option 524288, command 1048576, and 8388608 on arrow keys.
  system.defaults.CustomUserPreferences."com.apple.symbolichotkeys".AppleSymbolicHotKeys =
    let
      shortcut = enabled: character: keycode: modifiers: {
        inherit enabled;
        value = {
          parameters = [
            character
            keycode
            modifiers
          ];
          type = "standard";
        };
      };
    in
    {
      "32" = shortcut false 65535 126 8650752; # Mission Control, ctrl-up
      "33" = shortcut false 65535 125 8650752; # Application windows, ctrl-down
      "60" = shortcut false 32 49 262144; # Select the previous input source, ctrl-space
      "61" = shortcut false 32 49 786432; # Select the next input source, ctrl-option-space
      "64" = shortcut false 32 49 1048576; # Show Spotlight search, cmd-space
      "79" = shortcut false 65535 123 8650752; # Move left a space, ctrl-left
      "80" = shortcut true 65535 123 8781824; # stored with 79, ctrl-shift-left
      "81" = shortcut false 65535 124 8650752; # Move right a space, ctrl-right
      "82" = shortcut true 65535 124 8781824; # stored with 81, ctrl-shift-right

      "164" = shortcut false 65535 65535 0;
      "176" = {
        enabled = false;
        value.type = "SAE1.0";
      };
    };

  # The hotkey server reads these only at login; activateSettings applies them
  # to the running session.
  system.activationScripts.postActivation.text =
    let
      user = lib.escapeShellArg config.system.primaryUser;
    in
    lib.mkAfter ''
      launchctl asuser "$(id -u -- ${user})" sudo --user=${user} -- \
        /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u \
        || echo "warning: keyboard shortcuts apply at the next login" >&2
    '';
}
