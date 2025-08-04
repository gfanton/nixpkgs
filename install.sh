#!/usr/bin/env bash

# NixOS Cloud Installation Script
# Usage: curl https://raw.githubusercontent.com/gfanton/nixpkgs/main/install.sh | bash -s -- [ARCH] [DISK_LAYOUT]
# Example: curl https://raw.githubusercontent.com/gfanton/nixpkgs/main/install.sh | bash -s -- arm64 basic

set -euo pipefail

# Configuration
REPO="gfanton/nixpkgs"
BRANCH="feat/nixos"
FLAKE_URL="github:${REPO}/${BRANCH}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

# Check if running as root
check_root() {
    if [[ $EUID -ne 0 ]]; then
        error "This script must be run as root"
        error "Please run: sudo $0 $*"
        exit 1
    fi
}

# Auto-detect system architecture
detect_architecture() {
    local arch
    arch=$(uname -m)
    
    case "$arch" in
        "x86_64")
            echo "x86_64"
            ;;
        "aarch64"|"arm64")
            echo "arm64"
            ;;
        *)
            error "Unsupported architecture: $arch"
            error "Supported architectures: x86_64, aarch64/arm64"
            exit 1
            ;;
    esac
}

# Auto-detect disk layout preference
detect_disk_layout() {
    # Check available disk space to recommend layout
    local disk_size
    disk_size=$(df / | awk 'NR==2 {print $2}' 2>/dev/null || echo "0")
    
    # If disk is larger than 50GB, suggest LVM for flexibility
    if [[ $disk_size -gt 52428800 ]]; then # 50GB in KB
        echo "lvm"
    else
        echo "basic"
    fi
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."
    
    # Check if we're on a Linux system
    if [[ "$(uname -s)" != "Linux" ]]; then
        error "This script only works on Linux systems"
        exit 1
    fi
    
    # Check if Nix is installed
    if ! command -v nix &> /dev/null; then
        error "Nix is not installed. Please install Nix first:"
        error "curl -L https://nixos.org/nix/install | sh"
        exit 1
    fi
    
    # Check if flakes are enabled
    if ! nix flake --help &> /dev/null; then
        error "Nix flakes are not enabled. Please enable experimental features:"
        error "echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf"
        exit 1
    fi
    
    # Check internet connectivity
    if ! curl -sf --max-time 10 https://github.com &>/dev/null; then
        error "No internet connectivity or GitHub is unreachable"
        exit 1
    fi
    
    success "Prerequisites check passed"
}

# Determine target configuration
determine_config() {
    local arch="${1:-$(detect_architecture)}"
    local disk_layout="${2:-$(detect_disk_layout)}"
    
    log "Detected architecture: $arch"
    log "Detected disk layout: $disk_layout"
    
    # Map architecture names
    case "$arch" in
        "x86_64"|"x86"|"amd64")
            arch="x86"
            ;;
        "aarch64"|"arm64"|"arm")
            arch="arm"
            ;;
    esac
    
    # Determine flake configuration
    local config
    case "$disk_layout" in
        "lvm")
            config="cloud-lvm-${arch}"
            ;;
        "basic"|*)
            config="cloud-${arch}"
            ;;
    esac
    
    echo "$config"
}

# Validate flake configuration exists
validate_config() {
    local config="$1"
    
    log "Validating configuration: $config"
    
    if ! nix flake show "${FLAKE_URL}" --json 2>/dev/null | jq -e ".nixosConfigurations.\"${config}\"" >/dev/null; then
        error "Configuration '$config' not found in flake"
        error "Available configurations:"
        nix flake show "${FLAKE_URL}" 2>/dev/null | grep -E "├───|└───" | grep nixosConfigurations || true
        exit 1
    fi
    
    success "Configuration '$config' validated"
}

# Install NixOS using nixos-anywhere
install_nixos() {
    local config="$1"
    local target_host="${TARGET_HOST:-root@localhost}"
    
    log "Starting NixOS installation with configuration: $config"
    log "Target host: $target_host"
    
    # Check if nixos-anywhere is available
    if ! command -v nixos-anywhere &> /dev/null; then
        log "Installing nixos-anywhere..."
        nix profile install github:nix-community/nixos-anywhere
    fi
    
    # Run nixos-anywhere
    log "Running nixos-anywhere deployment..."
    log "This may take 10-30 minutes depending on network speed and system specs"
    
    if nixos-anywhere \
        --flake "${FLAKE_URL}#${config}" \
        --generate-hardware-config nixos-generate-config ./hardware-configuration.nix \
        "${target_host}"; then
        
        success "NixOS installation completed successfully!"
        success "Your system is now running NixOS with configuration: $config"
        
        log "You can rebuild your system with:"
        log "  nixos-rebuild switch --flake ${FLAKE_URL}#${config}"
        
        log "Or customize your configuration by cloning the repository:"
        log "  git clone https://github.com/${REPO}.git"
        log "  cd nixpkgs && git checkout ${BRANCH}"
        
    else
        error "NixOS installation failed"
        error "Check the logs above for error details"
        exit 1
    fi
}

# Main installation function
main() {
    local arch="${1:-}"
    local disk_layout="${2:-}"
    
    log "Starting NixOS Cloud Installation"
    log "Repository: ${REPO} (branch: ${BRANCH})"
    
    # Perform checks
    check_root
    check_prerequisites
    
    # Determine configuration
    local config
    config=$(determine_config "$arch" "$disk_layout")
    
    # Validate configuration exists
    validate_config "$config"
    
    # Confirm installation
    echo
    warn "This will COMPLETELY WIPE the target system and install NixOS"
    warn "Configuration: $config"
    warn "Target: ${TARGET_HOST:-localhost}"
    echo
    read -p "Are you sure you want to continue? (yes/no): " -r
    echo
    
    if [[ ! $REPLY =~ ^[Yy][Ee][Ss]$ ]]; then
        log "Installation cancelled by user"
        exit 0
    fi
    
    # Install NixOS
    install_nixos "$config"
}

# Usage information
usage() {
    cat << EOF
NixOS Cloud Installation Script

Usage: $0 [ARCH] [DISK_LAYOUT]

Arguments:
  ARCH         Target architecture (x86_64, arm64, auto-detect if omitted)
  DISK_LAYOUT  Disk partitioning layout (basic, lvm, auto-detect if omitted)

Environment Variables:
  TARGET_HOST    Target host for installation (default: root@localhost)

Examples:
  # Auto-detect everything and install on localhost
  $0

  # Install ARM64 configuration with LVM
  $0 arm64 lvm

  # Install on remote host with basic partitioning
  TARGET_HOST=root@192.168.1.100 $0 x86_64 basic

Available configurations:
  - cloud-x86, cloud-arm: Standard cloud configurations
  - cloud-lvm-x86, cloud-lvm-arm: LVM-based configurations for advanced storage

EOF
}

# Handle help flag
if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]]; then
    usage
    exit 0
fi

# Run main function with all arguments
main "$@"