# NixOS configuration for cloud VMs (KubeVirt/QEMU)
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  # Fetch SSH keys from GitHub (public, no secrets)
  githubKeys = builtins.fetchurl {
    url = "https://github.com/gfanton.keys";
    sha256 = "13bckv6bwkljly6g7vvldc0pd30avh7hl6k0fplbqmhzwcxnpjwc";
  };
in
{
  imports = [
    ./hardware.nix
  ];

  # System
  system.stateVersion = "24.11";

  # Boot
  boot.loader.grub = {
    enable = true;
    device = "/dev/vda";
  };

  # Networking
  networking = {
    hostName = "gfanton-dev";
    useDHCP = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
    };
  };

  # Timezone and locale
  time.timeZone = "UTC";
  i18n.defaultLocale = "en_US.UTF-8";

  # SSH
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # Nix settings
  nix = {
    package = pkgs.nix;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      substituters = [
        "https://cache.nixos.org"
        "https://gfanton.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "gfanton.cachix.org-1:i8zC+UjhhW5Wx2iRibhexJeBb1jOU/8oRFGG60IaAmI="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      trusted-users = [
        "root"
        "gfanton"
      ];
    };
  };

  # Enable zsh system-wide
  programs.zsh.enable = true;

  # User configuration
  users.users.gfanton = {
    isNormalUser = true;
    description = "Guillaume Fanton";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keyFiles = [ githubKeys ];
  };

  # Passwordless sudo for wheel group
  security.sudo.wheelNeedsPassword = false;

  # Essential packages
  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    wget
    htop
  ];
}
