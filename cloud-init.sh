#!/bin/sh
set -eu

# Simple cloud-init script for Hetzner Cloud
# Usage: curl -L https://gist.github.com/.../cloud-init.sh | sh

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"
}

log "Starting cloud-init setup..."

# Install Nix if not present
if ! command -v nix >/dev/null 2>&1; then
    log "Installing Nix..."
    curl -L https://nixos.org/nix/install | sh -s -- --daemon --yes || {
        log "ERROR: Failed to install Nix"
        exit 1
    }
    
    # Wait for nix daemon to be ready
    sleep 5
else
    log "Nix already installed"
fi

# Create gfanton user if not exists
if ! id -u gfanton >/dev/null 2>&1; then
    log "Creating user gfanton..."
    # Try to add to sudo group first (Debian/Ubuntu), fall back to wheel (RHEL/Fedora)
    if grep -q "^sudo:" /etc/group; then
        useradd -m -s /bin/bash -G sudo gfanton
    else
        useradd -m -s /bin/bash -G wheel gfanton
    fi
    
    # Allow passwordless sudo
    echo "gfanton ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/gfanton
    
    # Setup SSH keys
    log "Setting up SSH keys..."
    mkdir -p /home/gfanton/.ssh
    curl -sL https://github.com/gfanton.keys > /home/gfanton/.ssh/authorized_keys || {
        log "ERROR: Failed to fetch SSH keys from GitHub"
        exit 1
    }
    chmod 700 /home/gfanton/.ssh
    chmod 600 /home/gfanton/.ssh/authorized_keys
    chown -R gfanton:gfanton /home/gfanton/.ssh
else
    log "User gfanton already exists"
fi

# Add GitHub to known hosts
if [ ! -f /home/gfanton/.ssh/known_hosts ]; then
    log "Adding GitHub to known hosts..."
    sudo -u gfanton ssh-keyscan -H github.com >> /home/gfanton/.ssh/known_hosts
    chown gfanton:gfanton /home/gfanton/.ssh/known_hosts
fi

# Clone nixpkgs if not present
if [ ! -d /home/gfanton/nixpkgs ]; then
    log "Cloning nixpkgs configuration..."
    # Use HTTPS instead of SSH for initial clone
    sudo -u gfanton git clone https://github.com/gfanton/nixpkgs.git /home/gfanton/nixpkgs
    
else
    log "nixpkgs directory already exists"
fi

# Switch to configuration
log "Switching to nix configuration..."
cd /home/gfanton/nixpkgs

# Wait for nix daemon to be fully ready
sleep 10

# Ensure experimental features are enabled
log "Configuring Nix experimental features..."
mkdir -p /etc/nix
if ! grep -q "experimental-features" /etc/nix/nix.conf 2>/dev/null; then
    echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
fi
systemctl restart nix-daemon.service || true
sleep 5

# Run as gfanton with proper environment
log "Building and activating configuration..."
sudo -u gfanton -i bash -c 'cd ~/nixpkgs && make switch.cloud'

# Setup zsh as default shell after nix configuration
log "Setting up zsh as default shell..."
ZSH_PATH="/home/gfanton/.nix-profile/bin/zsh"
if [ -x "$ZSH_PATH" ]; then
    # Add zsh to /etc/shells if not already there
    if ! grep -q "$ZSH_PATH" /etc/shells; then
        echo "$ZSH_PATH" >> /etc/shells
        log "Added zsh to /etc/shells"
    fi
    
    # Change user's default shell
    usermod -s "$ZSH_PATH" gfanton
    log "Changed default shell to zsh for user gfanton"
else
    log "WARNING: zsh not found at $ZSH_PATH, keeping bash as default shell"
fi

log "Cloud-init setup completed!"
log "You can now SSH as: ssh gfanton@$(hostname -I | awk '{print $1}')"