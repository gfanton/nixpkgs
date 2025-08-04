# Base NixOS cloud configuration
# This is the shared base configuration for all cloud deployments
{ config, lib, pkgs, ... }:

{
  imports = [
    ./disk-configs/cloud-basic.nix
  ];

  # Boot loader configuration for cloud instances
  boot = {
    loader.grub = {
      enable = true;
      device = "/dev/sda";
      enableCryptodisk = false;
    };
    
    # Boot timeout
    loader.timeout = 1; # Faster boot for cloud
    
    # Cloud-optimized kernel parameters
    kernelParams = [
      "console=ttyS0,115200"
      "console=tty0"
    ];
    
    # Common cloud virtio modules
    kernelModules = [ "virtio_pci" "virtio_net" "virtio_blk" "virtio_scsi" ];
    
    # Enable BBR congestion control
    kernel.sysctl = {
      # Network performance optimizations
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
    
    # Automatic filesystem resize for cloud volumes
    growPartition = true;
  };

  # Network configuration for cloud instances
  networking = {
    useDHCP = false;
    enableIPv6 = true;
    
    # Common cloud networking setup
    interfaces = {
      # Primary interface (varies by provider: ens3, enp1s0, etc.)
      # Will be overridden in provider-specific configs
    };
  };

  # Systemd network configuration for cloud
  systemd.network = {
    enable = true;
    networks."10-wan" = {
      # Match common cloud interface names
      matchConfig.Name = "ens3 enp1s0 eth0";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
      
      # Cloud networking optimizations
      dhcpV4Config = {
        RouteMetric = 1024;
        UseMTU = true;
      };
      
      ipv6AcceptRAConfig = {
        UseMTU = true;
        UseGateway = true;
      };
      
      # Gateway configuration with onlink flag (required for many cloud providers)
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

  # Cloud-init configuration for various providers
  services.cloud-init = {
    enable = true;
    network.enable = true;
    settings = {
      # Support multiple datasources (auto-detection)
      datasource_list = [ "Hetzner" "DigitalOcean" "Amazon" "GCE" "Azure" "None" ];
      system_info = {
        distro = "nixos";
        network = {
          renderers = [ "networkd" ];
        };
      };
      users = [ "root" ];
      # Cloud modules for bootstrap
      cloud_init_modules = [
        "write-files"
        "growpart"
        "resizefs" 
        "users-groups"
        "ssh"
      ];
    };
  };

  # QEMU guest agent for better cloud integration
  services.qemuGuest.enable = true;

  # SSH configuration
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "yes";
    };
    openFirewall = true;
  };

  # Basic firewall for cloud security
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 ]; # SSH
    allowPing = true;
  };

  # Essential cloud packages
  environment.systemPackages = with pkgs; [
    curl
    wget
    git
    htop
    tmux
    vim
    tree
  ];

  # User configuration
  users.users.root = {
    openssh.authorizedKeys.keys = [
      # SSH keys will be added here or via cloud-init
    ];
  };

  # System configuration
  system.stateVersion = "25.05"; # NixOS release version
  
  # Enable nix flakes
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # Timezone and locale
  time.timeZone = "UTC";
  i18n.defaultLocale = "en_US.UTF-8";

  # Essential services for cloud
  services = {
    # Enable periodic TRIM for SSDs
    fstrim.enable = true;
    
    # Enable systemd timesyncd
    timesyncd.enable = true;
  };
}