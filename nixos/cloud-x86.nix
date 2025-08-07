# Complete x86_64 cloud configuration with home-manager
# Single command deployment: nix run github:numtide/nixos-anywhere -- --flake .#cloud-x86 root@IP
{ config, lib, pkgs, inputs, hostname, username, ... }:

{
  imports = [
    ./cloud.nix  # Complete cloud system configuration
    inputs.home-manager.nixosModules.home-manager  # System-level home-manager
  ];

  # x86_64-specific optimizations
  powerManagement.cpuFreqGovernor = lib.mkForce "performance";  # Better for x86_64

  # x86_64 typically uses GRUB (already configured in cloud.nix)
  # No boot loader overrides needed

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