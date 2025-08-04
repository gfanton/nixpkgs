{ pkgs, config, ... }:

{
  # Networking
  # networking.dns = [ ];

  # Apps
  # `home-manager` currently has issues adding them to `~/Applications`
  # Issue: https://github.com/nix-community/home-manager/issues/1341
  environment.systemPackages = with pkgs; [
    kitty
    terminal-notifier
    pam-reattach
  ];

  # https://github.com/nix-community/home-manager/issues/423
  environment.variables = {
    # TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
  };
  programs.nix-index.enable = true;

  # Fonts
  fonts.packages = with pkgs; [
    # recursive
    emacs-all-the-icons-fonts
    nerd-fonts.iosevka
    nerd-fonts.jetbrains-mono
    nerd-fonts.fira-code
    nerd-fonts.sauce-code-pro
  ];

  # Keyboard
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Custom emacs daemon service
  services.my-emacs =
    let
      primaryUser = config.users.primaryUser.username;
      homeDir = config.users.users.${primaryUser}.home;

      # Use standard Nix profile paths from environment.profiles pattern
      perUserProfile = "/etc/profiles/per-user/${primaryUser}";
      userNixProfile = "${homeDir}/.nix-profile";
      nixStateProfile = "${homeDir}/.local/state/nix/profile";
    in
    {
      enable = true;
      # Use the emacs package from home-manager user profile
      package = pkgs.writeShellScriptBin "emacs" ''
        exec ${perUserProfile}/bin/emacs "$@"
      '';
      # Add user profile paths for tools like ripgrep (following environment.profiles order)
      additionalPath = [
        "${perUserProfile}/bin"
        "${userNixProfile}/bin"
        "${nixStateProfile}/bin"
        "${homeDir}/.local/bin"
        "/opt/homebrew/bin"
        "/usr/local/bin"
      ];
    };

  # Add ability to used TouchID for sudo authentication with tmux support

  security.pam.services.sudo_local.text = ''
    # sudo_local: local config file which survives system update and is included for sudo
    # uncomment the following line to implicitly trust users in the admin group
    #auth       sufficient     pam_admin.so
    # if this isn't set darwin will fall back to NOPASSWD for wheel group
    auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so
    auth       sufficient     pam_tid.so
    auth       required       pam_opendirectory.so
    account    required       pam_permit.so
    password   required       pam_deny.so
    session    required       pam_permit.so
  '';
}
