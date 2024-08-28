{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Nix configuration ------------------------------------------------------------------------------

  nix.settings = {
    substituters = [
      "https://gfanton.cachix.org"
      "https://moul.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.nixos.org/"
    ];
    trusted-public-keys = [
      "gfanton.cachix.org-1:i8zC+UjhhW5Wx2iRibhexJeBb1jOU/8oRFGG60IaAmI="
      "moul.cachix.org-1:jcmTECmIfe9zam+p4sP3RhEXmH7QTTChd9ax/vo1CYs="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];

    trusted-users = [ "@admin" ];

    auto-optimise-store = false;

    experimental-features = [
      "nix-command"
      "flakes"
    ];

    keep-outputs = true;
    keep-derivations = true;

    extra-platforms = lib.mkIf (pkgs.system == "aarch64-darwin") [
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  };

  nix.configureBuildUsers = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  # Shells -----------------------------------------------------------------------------------------

  # Add shells installed by nix to /etc/shells file
  environment.shells = with pkgs; [
    bashInteractive
    zsh
  ];

  # Install and setup ZSH to work with nix(-darwin) as well
  programs.zsh.enable = true;
  environment.variables.SHELL = "${pkgs.zsh}/bin/zsh";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
