# Complete NixOS cloud configuration
# Universal configuration for all cloud providers and architectures
{ config, lib, pkgs, username ? "gfanton", hostname ? "nixos-cloud", ... }:

{
  imports = [
    ./disk-configs/cloud-basic.nix
    ./users.nix
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

  # Essential cloud system packages (single source of truth)
  environment.systemPackages = with pkgs; [
    # Core system tools
    curl
    wget
    git
    vim
    htop
    tree
    
    # Development tools available system-wide
    emacs-nox  # Terminal emacs for cloud environments
    tmux      # Terminal multiplexer
    
    # Build essentials
    gcc
    gnumake
    pkg-config
    
    # System utilities
    procps
    inetutils
  ];

  # User configuration
  users.users.root = {
    openssh.authorizedKeys.keys = [
      # SSH keys will be added here or via cloud-init
    ];
  };

  # Enable ZSH system-wide
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # System hostname
  networking.hostName = hostname;

  # Architecture-specific optimizations
  powerManagement = {
    enable = true;
    # Architecture-specific CPU governor (will be overridden in specific configs)
    cpuFreqGovernor = lib.mkDefault "ondemand";
  };

  # Comprehensive kernel optimizations for cloud environments
  boot.kernel.sysctl = {
    # Network performance optimizations
    "net.core.rmem_max" = 16777216;
    "net.core.wmem_max" = 16777216;
    "net.ipv4.tcp_rmem" = "4096 87380 16777216";
    "net.ipv4.tcp_wmem" = "4096 16384 16777216";
    "net.ipv4.tcp_congestion_control" = "bbr";
    
    # Memory management optimizations
    "vm.dirty_ratio" = 15;
    "vm.dirty_background_ratio" = 5;
    "vm.swappiness" = 1;
    
    # Scheduler optimizations
    "kernel.sched_autogroup_enabled" = 1;
    "kernel.sched_migration_cost_ns" = 5000000;
    
    # x86_64-specific optimization (no-op on ARM64)
    "kernel.numa_balancing" = lib.mkDefault 1;
  };

  # Comprehensive Nix configuration for development
  nix = {
    settings = {
      # Enable flakes and new commands
      experimental-features = [ "nix-command" "flakes" ];
      
      # Automatic store optimization
      auto-optimise-store = true;
      
      # Trusted users for remote builds
      trusted-users = [ "root" username ];
      
      # Performance optimizations
      max-jobs = "auto";
      cores = 0; # Use all available cores
    };
    
    # Automatic garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Locale and timezone settings
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "UTC"; # Standard for cloud servers
  
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # System state version
  system.stateVersion = "25.05";
}
