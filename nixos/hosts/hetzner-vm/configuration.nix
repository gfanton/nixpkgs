# NixOS configuration for Hetzner Cloud ARM64 virtual machines
# Based on: https://www.alexghr.me/blog/hetzner-nixos-server/
# Adapted for our nixpkgs setup with ARM64 support

{ config, lib, pkgs, hostname, username, ... }:

{
  # Module imports handled by flake.nix

  # System identification
  networking.hostName = hostname;
  system.stateVersion = "25.05";

  # Timezone and locale
  time.timeZone = "UTC";  # Keep UTC for cloud servers
  i18n.defaultLocale = "en_US.UTF-8";

  # Boot configuration for Hetzner Cloud
  boot = {
    loader.grub = {
      # no need to set devices, disko will add all devices that have a EF02 partition to the list already
      # devices = [ ];
      efiSupport = true;
      efiInstallAsRemovable = true;
    };

    # Kernel parameters for console access
    kernelParams = [ 
      "console=tty0"
      "console=ttyS0,115200"
    ];

    # Essential kernel modules for Hetzner Cloud
    kernelModules = [ ];
    extraModulePackages = [ ];
  };

  # Network configuration optimized for Hetzner Cloud
  networking = {
    useDHCP = false;
    
    # Firewall configuration - more restrictive than the tutorial
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];  # Only SSH
      allowPing = true;
    };
  };

  # Systemd network configuration for Hetzner Cloud
  systemd.network = {
    enable = true;
    networks."10-wan" = {
      # Match any ethernet interface for better compatibility
      matchConfig.Name = "e*";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
      # Simplified IPv6 gateway configuration
      routes = [
        {
          routeConfig = {
            Gateway = "fe80::1";
            GatewayOnLink = true;
          };
        }
      ];
    };
  };

  # SSH configuration
  services.openssh = {
    enable = true;
    ports = [ 22 ];
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "yes";
    };
    extraConfig = ''
      PrintLastLog no
      ClientAliveInterval 60
      ClientAliveCountMax 3
    '';
  };

  # Security enhancements - disabled for initial testing
  # services.fail2ban = {
  #   enable = true;
  #   maxretry = 5;
  #   bantime = "24h";
  # };

  # QEMU guest agent for better cloud integration
  services.qemuGuest.enable = true;

  # Nix configuration
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # Essential system packages only
  # User packages should be managed in home-manager
  environment.systemPackages = with pkgs; [
    curl
    wget
    git
    vim
    emacs-nox  # Basic Emacs without X11
    htop
    tmux
  ];

  # System services
  services = {
    # Enable periodic TRIM for SSDs
    fstrim.enable = true;
    
    # Enable systemd timesyncd
    timesyncd.enable = true;
  };

  # Network performance optimizations
  boot.kernel.sysctl = {
    # Network performance
    "net.core.rmem_max" = 16777216;
    "net.core.wmem_max" = 16777216;
    "net.ipv4.tcp_rmem" = "4096 87380 16777216";
    "net.ipv4.tcp_wmem" = "4096 16384 16777216";
    "net.ipv4.tcp_congestion_control" = "bbr";
    
    # Storage optimizations
    "vm.dirty_ratio" = 15;
    "vm.dirty_background_ratio" = 5;
    "vm.swappiness" = 1;
  };
}