# NixOS Cloud Configurations

This directory contains NixOS configurations optimized for cloud deployment with support for ARM64 and x86_64 architectures.

## Quick Start

**Single-command deployment:**
```bash
curl https://raw.githubusercontent.com/gfanton/nixpkgs/feat/nixos/install.sh | bash
```

**Manual deployment with nixos-anywhere:**
```bash
# Standard cloud deployment (works on all major providers)
nixos-anywhere --flake github:gfanton/nixpkgs/feat/nixos#cloud-arm root@<host>
nixos-anywhere --flake github:gfanton/nixpkgs/feat/nixos#cloud-x86 root@<host>

# LVM-based deployment for advanced storage
nixos-anywhere --flake github:gfanton/nixpkgs/feat/nixos#cloud-lvm-arm root@<host>
nixos-anywhere --flake github:gfanton/nixpkgs/feat/nixos#cloud-lvm-x86 root@<host>
```

## Configuration Files

### Base Configurations

- **`cloud.nix`**: Universal cloud configuration with multi-provider support
  - SSH hardening (key-only authentication)
  - Multi-provider cloud-init integration (AWS, GCP, Azure, Hetzner, DigitalOcean)
  - QEMU guest agent for better virtualization support
  - Performance optimizations (BBR congestion control, virtio modules)
  - Comprehensive networking setup with automatic interface detection

### Architecture-Specific Configurations

- **`cloud-arm.nix`**: ARM64 optimizations
  - UEFI boot configuration for ARM64
  - Network performance tuning for ARM processors
  - ARM-specific hardware optimizations
  - BBR congestion control

- **`cloud-x86.nix`**: x86_64 optimizations
  - GRUB boot configuration
  - Intel/AMD microcode updates
  - Thermal management
  - x86-specific performance tuning

### Cloud Integration Features

The base configuration includes optimizations for all major cloud providers:
- **Multi-provider cloud-init support**: Auto-detects Hetzner, AWS, GCP, Azure, DigitalOcean
- **Universal networking**: Handles common cloud interface names (ens3, enp1s0, eth0)
- **Gateway configuration**: Includes onlink flags required by many cloud providers
- **Virtio optimization**: Full virtio driver support for better performance
- **Auto-resizing**: Automatic filesystem expansion when volumes are expanded

### Disk Configurations

- **`disk-configs/cloud-basic.nix`**: Simple GPT partitioning
  - UEFI/BIOS compatible
  - Single root partition with optimized mount options
  - SSD-optimized filesystem settings

- **`disk-configs/cloud-lvm.nix`**: LVM-based partitioning
  - Flexible volume management
  - Separate root, home, and swap volumes
  - Easy expansion and management

## Available NixOS Configurations

| Configuration | Architecture | Cloud Providers | Disk Layout | Use Case |
|---------------|-------------|----------------|-------------|-----------|
| `cloud-x86` | x86_64 | Universal (AWS, GCP, Azure, Hetzner, DigitalOcean, etc.) | Basic GPT | Standard cloud instances |
| `cloud-arm` | ARM64 | Universal (AWS Graviton, Hetzner ARM, etc.) | Basic GPT | ARM cloud instances |
| `cloud-lvm-x86` | x86_64 | Universal | Advanced LVM | Complex storage requirements |
| `cloud-lvm-arm` | ARM64 | Universal | Advanced LVM | ARM instances with advanced storage |

## Key Features

### Security
- SSH key-only authentication (password auth disabled)
- Minimal firewall with only SSH port open
- Automatic security updates with rollback capability
- Microcode updates for x86_64 systems

### Performance
- Architecture-specific kernel optimizations
- SSD-optimized mount options (noatime, commit intervals)
- BBR congestion control for network performance
- CPU frequency scaling optimized per architecture

### Cloud Integration
- Cloud-init for initial setup and metadata
- Provider-specific networking configurations
- Automatic filesystem resizing
- QEMU guest agent for virtualized environments

### Management
- Declarative disk partitioning with disko
- Automatic garbage collection
- NixOS generations for easy rollbacks
- Flake-based configuration management

## Customization

To customize configurations for your needs:

1. **Fork the repository**
2. **Modify configurations** in this directory
3. **Add SSH keys** to the appropriate configuration files
4. **Deploy with your forked flake:**
   ```bash
   nixos-anywhere --flake github:your-username/nixpkgs/your-branch#cloud-arm root@<host>
   ```

## Testing

Test configurations locally before deployment:

```bash
# Build configuration
nix build .#nixosConfigurations.cloud-arm.config.system.build.toplevel

# Check configuration
nix flake check

# Show available configurations  
nix flake show
```

## Cloud Provider Support

This configuration is designed to work universally across all major cloud providers:

- **✅ AWS** (including Graviton ARM instances)
- **✅ Google Cloud Platform** 
- **✅ Microsoft Azure**
- **✅ Hetzner Cloud** (x86_64 and ARM64)
- **✅ DigitalOcean**
- **✅ Vultr, Linode, and other VPS providers**

The configuration automatically detects cloud metadata services and adapts accordingly.

For issues or improvements, please open an issue in the main repository.