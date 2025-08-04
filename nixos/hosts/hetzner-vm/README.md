# Complete NixOS Cloud Development Environment

This configuration provides a **complete development environment** deployed to Hetzner Cloud (or any cloud provider) with your full nixpkgs configuration including home-manager integration.

## What You Get

- **Complete NixOS system** with all your development tools
- **Home-manager integration** with your personal package configuration  
- **Development tools**: tmux, emacs, git, shells (zsh), and all your custom packages
- **Cloud-optimized**: LVM storage, systemd-networkd, security hardening
- **Single-command deployment** from any machine with Nix

## Features

- **One-command deployment** using nixos-anywhere
- **Complete home-manager integration** with your existing configuration
- **LVM disk layout** for flexibility and reliability  
- **ARM64 & x86_64 support** for all major cloud providers
- **Security focused** with SSH key authentication only
- **Auto-updates** with Nix garbage collection

## Complete Setup Guide (A to Z)

### Step 1: Prerequisites

**Local Machine Requirements:**
- Any machine with Nix installed (macOS, Linux, or WSL)
- SSH access to create cloud instances

**Cloud Provider Setup (Hetzner Cloud example):**
1. **Create a Hetzner Cloud server:**
   - **Location**: Any region
   - **Image**: Ubuntu 22.04 (will be completely replaced by NixOS)
   - **Type**: CAX11 (ARM64) or CPX11 (x86_64) - minimum for testing
   - **SSH Key**: Add your SSH public key during creation ⚠️ **CRITICAL**
   - **Networking**: Default settings (public IPv4/IPv6)

2. **Verify your SSH key:**
   ```bash
   # Your SSH key MUST match what's configured in nixos/users.nix
   cat ~/.ssh/id_ed25519.pub
   # Should output: ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDFmNJ0xGEEzdA/Dx7S4ySc1/LA1r2KqosH4SuKKsnca guilhem.fanton@gmail.com
   ```

### Step 2: One-Command Deployment

**Deploy your complete development environment:**
```bash
# Get the latest commit hash to avoid caching issues
git log --oneline -1

# Deploy to your cloud instance (replace YOUR_IP with actual IP)
nix run github:numtide/nixos-anywhere -- --flake github:gfanton/nixpkgs/COMMIT_HASH#hetzner-vm root@YOUR_IP
```

**What happens during deployment:**
- Downloads NixOS kexec installer (~340MB)
- Boots into NixOS installer environment  
- Formats disk with LVM layout
- Builds complete NixOS system with home-manager
- Installs all your development tools and configurations
- Reboots into your personalized NixOS environment

**⏱️ Timing:** Full deployment takes 8-12 minutes on ARM64 instances.

### Step 3: Access Your Development Environment

**Connect to your new system:**
```bash
# Remove old host key if reusing IP
ssh-keygen -f ~/.ssh/known_hosts -R "YOUR_IP"

# Wait ~90 seconds for boot, then connect
ssh gfanton@YOUR_IP
```

**Verify everything is working:**
```bash
# Check system info
hostname          # Should show: vanille
whoami            # Should show: gfanton  
uname -a          # Should show: Linux vanille 6.12.x ... aarch64 GNU/Linux

# Test your development tools
tmux --version    # tmux from home-manager
emacs --version   # emacs-nox from home-manager
git --version     # git with your configuration
tree --version    # all your packages available

# Check configuration paths
echo $PATH        # Should include /etc/profiles/per-user/gfanton/bin
ls ~/.config      # Should show home-manager generated configs
```
### Step 4: Updating Your Configuration

**Making changes to your setup:**
```bash
# 1. Make changes to your configuration files locally
# 2. Commit and push your changes
git add -A && git commit -m "update: your changes"
git push

# 3. Get the new commit hash
git log --oneline -1

# 4. Update your cloud system
ssh gfanton@YOUR_IP 'sudo nixos-rebuild switch --flake github:gfanton/nixpkgs/NEW_COMMIT_HASH#hetzner-vm'
```

**Adding more home-manager modules:**
Currently configured with basic packages. To add more modules, edit `flake.nix`:
```nix
# In flake.nix, add more modules to the imports list:
imports = [
  self.homeManagerModules.home-user-info
  self.homeManagerModules.my-git
  self.homeManagerModules.my-shells      # Add shell configuration
  self.homeManagerModules.my-packages    # Add full package set  
  self.homeManagerModules.my-emacs       # Add emacs configuration
  # ... add more as needed
];
```

## Troubleshooting

### Common Issues

**❌ Deployment fails with "SSH key authentication failed"**
```bash
# Check your SSH key matches exactly what's in nixos/users.nix
cat ~/.ssh/id_ed25519.pub
# Must match: ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDFmNJ0xGEEzdA/Dx7S4ySc1/LA1r2KqosH4SuKKsnca guilhem.fanton@gmail.com
```

**❌ Build fails with "evaluation error" or missing modules**
```bash
# Use latest commit hash to bypass GitHub caching
git log --oneline -1
nix run github:numtide/nixos-anywhere -- --flake github:gfanton/nixpkgs/LATEST_COMMIT#hetzner-vm root@YOUR_IP
```

**❌ SSH connection times out after deployment**
- **ARM64 instances**: Wait 2-3 minutes for full boot
- **Check console**: Hetzner console should show "vanille login:" prompt
- **Network check**: Test from Hetzner console: `ping 8.8.8.8`

**❌ Home-manager packages not in PATH**
```bash
# Check if home-manager profile is loaded
echo $PATH | grep per-user
# Should include: /etc/profiles/per-user/gfanton/bin

# If missing, check home-manager service
systemctl --user status home-manager-gfanton.service
```

### Advanced Operations

**🔄 Updating System Configuration**
```bash
# Quick system update with latest config
ssh gfanton@YOUR_IP 'sudo nixos-rebuild switch --flake github:gfanton/nixpkgs/feat/nixos#hetzner-vm'
```

**🗑️ Cleaning Old Generations**
```bash
# Clean up old system generations to save space
ssh gfanton@YOUR_IP 'sudo nix-collect-garbage -d'
```

**📊 System Information**
```bash
# Check system info
ssh gfanton@YOUR_IP '
  echo "=== System Info ==="
  hostname && whoami && uname -a
  echo "=== NixOS Version ==="
  nixos-version
  echo "=== Available Tools ==="
  which tmux emacs git tree curl wget
  echo "=== Home-Manager Profile ==="
  ls -la /etc/profiles/per-user/gfanton/bin/ | head -10
'
```

## Customization & Configuration

### Personalizing Your Setup

**🏷️ Change hostname:**
```bash
# Edit flake.nix specialArgs
hostname = "your-name";  # Change from "vanille" to your preferred name

# Then commit and redeploy
git add -A && git commit -m "change hostname to your-name"
git push
# Deploy with new commit hash
```

**👤 Change username (advanced):**
```bash
# Edit flake.nix primaryUserInfo
primaryUserInfo = {
  username = "yourname";           # Change from "gfanton"
  email = "your@email.com";        # Your email
  fullName = "Your Full Name";
  nixConfigDirectory = "/home/yourname/nixpkgs";
};

# Also update nixos/users.nix with your SSH key and username
```

**📦 Add Your Own Packages:**
```bash
# Edit the home-manager configuration in flake.nix
home.packages = with pkgs; [
  # Current packages
  tmux emacs-nox git zsh curl wget tree htop
  
  # Add your tools
  docker
  nodejs
  python3
  # ... any package from nixpkgs
];
```

### Production Deployment

**🚀 Ready for Production Use:**
- ✅ **Secure**: SSH key authentication only, no passwords
- ✅ **Automated**: Declarative configuration with Nix
- ✅ **Reproducible**: Same environment on any cloud provider
- ✅ **Scalable**: Easy to spin up multiple identical instances
- ✅ **Maintainable**: Version controlled configuration

**🔒 Security Recommendations:**
- Use strong SSH keys (ed25519 recommended)
- Regularly update with `nixos-rebuild switch`
- Monitor with `journalctl -f` for system logs
- Use `nix-collect-garbage` to clean old generations

**💰 Cost Optimization:**
- **Hetzner CAX11**: ~€3.29/month (ARM64, 1 vCPU, 4GB RAM)
- **Hetzner CPX11**: ~€4.15/month (x86_64, 1 vCPU, 2GB RAM)
- Other cloud providers: Similar pricing for basic instances

## Alternative Cloud Providers

This configuration works on **any cloud provider**, not just Hetzner:

**🌩️ AWS EC2:**
```bash
nix run github:numtide/nixos-anywhere -- --flake github:gfanton/nixpkgs/COMMIT#hetzner-vm root@YOUR_EC2_IP
```

**🔵 DigitalOcean:**
```bash  
nix run github:numtide/nixos-anywhere -- --flake github:gfanton/nixpkgs/COMMIT#hetzner-vm root@YOUR_DROPLET_IP
```

**☁️ Google Cloud, Azure, Vultr, Linode:**
Same command works everywhere! Just replace the IP address.

## What Makes This Special

### 🎯 **Complete Development Environment**
- Not just a basic NixOS install - your **complete** personalized setup
- All your packages, configurations, and development tools ready to use
- Home-manager integration with your existing configurations

### 🚀 **One-Command Deployment**  
- From any machine with Nix: `nix run github:numtide/nixos-anywhere -- --flake github:gfanton/nixpkgs/COMMIT#hetzner-vm root@IP`
- No complex setup scripts, no manual configuration
- Reproducible across all cloud providers

### 🔒 **Production Ready**
- Security hardened with SSH keys only
- LVM storage for flexibility
- Automated updates with Nix
- Version controlled infrastructure

### 💡 **Nix-Powered**
- Declarative configuration - no configuration drift
- Rollback capability if something breaks  
- Reproducible builds - same environment everywhere
- Massive package ecosystem via nixpkgs

## Next Steps

1. **Deploy**: Follow the A-Z guide above
2. **Customize**: Add your packages and configurations  
3. **Scale**: Deploy to multiple instances easily
4. **Maintain**: Update with simple `nixos-rebuild switch`

## Technical Details

**Architecture:** Cloud-agnostic NixOS with home-manager integration  
**Storage:** LVM with ext4 filesystem  
**Network:** systemd-networkd with DHCP  
**Security:** SSH key authentication, fail2ban protection  
**Package Management:** Nix with home-manager for user packages

## References & Credits

This configuration builds upon excellent work from the NixOS community:

- **[nixos-anywhere](https://github.com/nix-community/nixos-anywhere)** - Zero-configuration NixOS deployment tool
- **[Alex Gherghisan's Tutorial](https://www.alexghr.me/blog/hetzner-nixos-server/)** - Hetzner Cloud NixOS setup
- **[NixOS Wiki](https://nixos.wiki/wiki/Install_NixOS_on_Hetzner_Cloud)** - Community knowledge base
- **[home-manager](https://github.com/nix-community/home-manager)** - Declarative user environment management

---

**🎉 You now have a complete, production-ready NixOS development environment that can be deployed to any cloud provider with a single command!**