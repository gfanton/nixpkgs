# Complete ARM64 cloud configuration (simplified to avoid circular dependencies)
# Single command deployment: nix run github:numtide/nixos-anywhere -- --flake .#cloud-arm root@IP
{ config, lib, pkgs, inputs, hostname ? "nixos-cloud", username ? "gfanton", ... }:

{
  imports = [
    ./cloud.nix  # Complete cloud system configuration
  ];

  # ARM64-specific optimizations
  powerManagement.cpuFreqGovernor = lib.mkForce "ondemand";  # Better for ARM64

  # ARM64 boot configuration - override GRUB settings from cloud.nix for ARM64
  boot.loader.grub = {
    efiSupport = lib.mkForce true;
    efiInstallAsRemovable = lib.mkForce true;
    device = lib.mkForce "nodev"; # For EFI systems
  };
}