# NixOS configuration for KubeVirt/QEMU cloud VMs
# Used with: nixos-rebuild switch --flake ~/nixpkgs#cloud-vm
{
  config,
  lib,
  pkgs,
  modulesPath,
  inputs,
  ...
}:
let
  # Fetch SSH keys from GitHub (public, no secrets in repo)
  githubKeys = builtins.fetchurl {
    url = "https://github.com/gfanton.keys";
    sha256 = "13bckv6bwkljly6g7vvldc0pd30avh7hl6k0fplbqmhzwcxnpjwc";
  };
in
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  system.stateVersion = "24.11";

  # Boot configuration for running VM
  boot.loader.grub = {
    enable = true;
    device = "/dev/vda";
  };

  # Virtio drivers for KubeVirt
  boot.initrd.availableKernelModules = [
    "virtio_pci"
    "virtio_blk"
    "virtio_scsi"
    "virtio_net"
    "ahci"
    "xhci_pci"
    "sr_mod"
  ];
  services.qemuGuest.enable = true;

  # Filesystem
  fileSystems."/" = lib.mkDefault {
    device = "/dev/vda1";
    fsType = "ext4";
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

  # SSH server
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # Mosh server for mobile shell connections
  programs.mosh.enable = true;

  # Nix configuration with Cachix
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
    # Enable lingering for user services (emacs daemon)
    linger = true;
  };

  # Passwordless sudo for wheel group
  security.sudo.wheelNeedsPassword = false;

  # Essential system packages
  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    wget
    htop
    tmux
  ];

  # Hardware platform
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
