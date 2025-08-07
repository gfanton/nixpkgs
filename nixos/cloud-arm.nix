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

  # Home-manager integration using homeManagerModules from flake
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs; };
    
    users.${username} = {
      imports = [
        # Use predefined home modules from flake
        inputs.self.homeManagerModules.my-packages
        inputs.self.homeManagerModules.my-shells  
        inputs.self.homeManagerModules.my-git
        inputs.self.homeManagerModules.my-emacs
        inputs.self.homeManagerModules.my-kitty
        inputs.self.homeManagerModules.programs-kitty-extras
        inputs.self.homeManagerModules.programs-truecolor
        inputs.self.homeManagerModules.programs-zsh-oh-my-zsh-extra
        inputs.self.homeManagerModules.home-user-info
        inputs.self.commonModules.colors
        inputs.self.commonModules.my-colors
        
        # Linux-specific configuration
        ({ config, lib, pkgs, ... }: {
          home = {
            username = username;
            homeDirectory = "/home/${username}";
            stateVersion = "25.11";
            user-info = {
              username = "Guilhem Fanton";
              email = "guilhem@gfanton.com";
              nixConfigDirectory = "/home/${username}/nixpkgs";
            };
          };
        })
      ];
    };
  };
}