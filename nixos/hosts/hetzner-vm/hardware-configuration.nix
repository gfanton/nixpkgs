# Hardware configuration for Hetzner Cloud virtual machines
# Based on: https://github.com/LGUG2Z/nixos-hetzner-cloud-starter
# Simplified and optimized for both x86_64 and ARM64

{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  # Kernel modules for Hetzner Cloud VMs
  boot.initrd.availableKernelModules = [ 
    "ahci" 
    "xhci_pci" 
    "virtio_pci" 
    "virtio_net"
    "virtio_blk"
    "virtio_scsi"
    "sd_mod" 
    "sr_mod" 
  ];
  
  # ARM64 specific modules for Hetzner Cloud CAX instances
  boot.initrd.kernelModules = lib.optionals (pkgs.stdenv.hostPlatform.isAarch64) [ 
    "virtio_gpu"  # Fix for blank screen on ARM64 CAX instances
  ];
  
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # Hardware optimizations
  hardware = {
    # Enable firmware updates if available
    enableAllFirmware = true;
  };

  # Network configuration handled by systemd-networkd in main config
  networking.useDHCP = lib.mkDefault false;

  # Platform specific settings - let flake.nix handle this
  # nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";

  # Power management for cloud instances
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  
  # Disable services not needed in cloud VMs
  services.thermald.enable = false;  # Not needed for cloud instances
}