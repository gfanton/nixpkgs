# x86_64-specific cloud configuration  
# Optimized for x86_64 cloud instances (most cloud providers)
{ config, lib, pkgs, ... }:

{
  imports = [
    ./cloud.nix
  ];

  # x86_64-specific boot configuration
  boot = {
    # GRUB is more common on x86_64 cloud instances
    loader.grub = {
      enable = true;
      device = "/dev/sda";
      enableCryptodisk = false;
    };
    
    # x86_64 kernel optimizations
    kernelParams = [
      "console=ttyS0,115200"
      "console=tty0"
    ];
  };

  # x86_64-specific network interface naming
  systemd.network.networks."10-wan" = {
    matchConfig.Name = "ens3"; # Common x86_64 interface name
    networkConfig = {
      DHCP = "ipv4";
      IPv6AcceptRA = true;
    };
  };

  # x86_64-optimized packages
  environment.systemPackages = with pkgs; [
    # x86_64 has good support for these tools
    intel-gpu-tools  # Intel graphics utilities
    lscpu           # CPU information
    dmidecode       # Hardware information
  ] ++ config.environment.systemPackages;

  # x86_64-specific hardware optimizations
  hardware = {
    enableAllFirmware = true;
    # CPU microcode updates
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };

  # Performance optimizations for x86_64
  powerManagement = {
    enable = true;
    cpuFreqGovernor = lib.mkDefault "performance"; # x86_64 can handle higher performance
  };

  # Thermal management for x86_64 (useful for some cloud providers)
  services.thermald.enable = lib.mkDefault true;
  
  # x86_64 cloud instances benefit from these settings
  boot.kernel.sysctl = {
    # Network performance optimizations
    "net.core.rmem_max" = 134217728;
    "net.core.wmem_max" = 134217728;
    "net.ipv4.tcp_rmem" = "4096 65536 134217728";
    "net.ipv4.tcp_wmem" = "4096 65536 134217728";
    "net.ipv4.tcp_congestion_control" = "bbr";
    
    # x86_64-specific optimizations
    "vm.swappiness" = 1; # Prefer RAM over swap
    "vm.dirty_ratio" = 15;
    "vm.dirty_background_ratio" = 5;
  };

  # Enable virtualization features commonly available on x86_64
  boot.kernelModules = [ "kvm-intel" "kvm-amd" ];
  virtualisation.libvirtd.enable = lib.mkDefault false; # Only if needed
}