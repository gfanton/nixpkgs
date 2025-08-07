# Complete ARM64 cloud configuration with home-manager
# Single command deployment: nix run github:numtide/nixos-anywhere -- --flake .#cloud-arm root@IP
{ config, lib, pkgs, inputs, hostname, username, ... }:

{
  imports = [
    ./cloud.nix  # Complete cloud system configuration
    inputs.home-manager.nixosModules.home-manager  # System-level home-manager
  ];

  # ARM64-specific optimizations
  powerManagement.cpuFreqGovernor = lib.mkForce "ondemand";  # Better for ARM64

  # ARM64 boot configuration (UEFI preferred for ARM64 cloud instances)
  boot.loader = {
    systemd-boot.enable = lib.mkForce true;
    grub.enable = lib.mkForce false;
    efi.canTouchEfiVariables = true;
  };

  # Home-manager integration (user packages and configs from home/ modules)
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs; };
    
    users.${username} = {
      imports = [
        # Single source of truth: all user config from home/ modules
        ../home/packages.nix
        ../home/shells.nix
        ../home/git.nix
        ../home/emacs.nix
        ../home/kitty.nix
        ../modules/home/colors
        
        # Minimal Linux-specific user info
        ({ config, lib, pkgs, ... }: {
          home = {
            username = username;
            homeDirectory = "/home/${username}";
            stateVersion = "25.11";
          };
        })
      ];
    };
  };
}