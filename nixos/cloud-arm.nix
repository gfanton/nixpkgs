# ARM64-specific cloud configuration
# Optimized for ARM64 cloud instances (Hetzner Cloud ARM, AWS Graviton, etc.)
{ config, lib, pkgs, ... }:

{
  imports = [
    ./cloud.nix
    ./users.nix
  ];

  # ARM64-specific boot configuration for cloud instances
  boot = {
    # ARM64 cloud instances use UEFI boot
    loader = {
      systemd-boot.enable = lib.mkForce true;
      grub.enable = lib.mkForce false;
      efi.canTouchEfiVariables = true;
    };
    
    # Override timeout from base config
    loader.timeout = lib.mkForce 3;
    
    # ARM64 kernel optimizations with better console support
    kernelParams = [
      "console=tty0"           # VGA console for Hetzner Cloud console
      "console=ttyS0,115200"   # Serial console
      "earlyprintk=serial,ttyS0,115200"
    ];
  };

  # ARM64-specific network interface naming (override base settings)
  systemd.network.networks."10-wan" = {
    matchConfig.Name = lib.mkForce "enp1s0"; # Common ARM64 interface name
    networkConfig = {
      DHCP = "ipv4";
      IPv6AcceptRA = true;
    };
  };

  # ARM64-optimized packages
  environment.systemPackages = with pkgs; [
    # ARM64 performs well with these tools
    btop      # Better than htop on ARM
    ripgrep   # Fast grep alternative
    fd        # Fast find alternative
    zoxide    # Smart cd command
  ];

  # ARM64-specific hardware optimizations
  hardware = {
    # Enable firmware updates if available
    enableAllFirmware = true;
    # CPU microcode updates (if available for ARM)
    cpu.amd.updateMicrocode = false; # Not applicable for ARM
    cpu.intel.updateMicrocode = false; # Not applicable for ARM
  };

  # Performance optimizations for ARM64
  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand"; # Good balance for cloud workloads
  };

  # Specific to Hetzner Cloud ARM instances
  # Ampere Altra processors work well with these settings
  services.thermald.enable = false; # Not needed for cloud instances
  
  # ARM64-specific sysctl optimizations (override base cloud settings)
  boot.kernel.sysctl = {
    # Enhanced network performance for ARM64 cloud instances
    "net.core.rmem_max" = lib.mkForce 134217728;
    "net.core.wmem_max" = lib.mkForce 134217728;
    "net.ipv4.tcp_rmem" = lib.mkForce "4096 65536 134217728";
    "net.ipv4.tcp_wmem" = lib.mkForce "4096 65536 134217728";
  };
}