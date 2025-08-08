#!/usr/bin/env bash

# Minimal NixOS Cloud Installation Script
# Usage: curl https://raw.githubusercontent.com/gfanton/nixpkgs/feat/nixos/install.sh | bash
set -euo pipefail

# Configuration
REPO="gfanton/nixpkgs"
BRANCH="feat/nixos"
FLAKE_URL="github:${REPO}/${BRANCH}"

# Timestamped logging function
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*"
}

# Auto-detect architecture and config
detect_config() {
    local arch=$(uname -m)
    case "$arch" in
        "x86_64") echo "cloud-x86" ;;
        "aarch64"|"arm64") echo "cloud-arm" ;;
        *) log "ERROR: Unsupported architecture: $arch"; exit 1 ;;
    esac
}

# Main installation
main() {
    local config=$(detect_config)
    
    log "Starting minimal NixOS installation"
    log "Architecture: $(uname -m)"
    log "Configuration: $config"
    log "Flake: $FLAKE_URL#$config"
    
    # Check root
    if [[ $EUID -ne 0 ]]; then
        log "ERROR: Must run as root"
        exit 1
    fi
    
    # Install Nix if needed
    if ! command -v nix &>/dev/null; then
        log "Installing Nix..."
        curl -L https://install.determinate.systems/nix | sh -s -- install --no-confirm
        source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        log "Nix installed"
    fi
    
    # Install nixos-anywhere
    if ! command -v nixos-anywhere &>/dev/null; then
        log "Installing nixos-anywhere..."
        nix profile install github:nix-community/nixos-anywhere
        log "nixos-anywhere installed"
    fi
    
    # Get current IP for reconnection info
    local ip=$(hostname -I | awk '{print $1}' || echo "localhost")
    
    log "Starting installation - system will reboot automatically"
    log "After reboot, reconnect with: ssh gfanton@$ip"
    log "Running: nixos-anywhere --flake $FLAKE_URL#$config --no-reboot root@localhost"
    
    # Run installation with no reboot flag so we can see completion
    if nixos-anywhere --flake "$FLAKE_URL#$config" --no-reboot root@localhost; then
        log "Installation completed successfully!"
        log "Rebooting into NixOS..."
        reboot
    else
        log "ERROR: Installation failed - check logs above"
        exit 1
    fi
}

main "$@"