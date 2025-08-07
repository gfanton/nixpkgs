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

# Install Nix if not present
install_nix() {
    log "🔧 Installing Nix package manager..."
    
    # Use the Determinate Nix Installer (more reliable and includes flakes by default)
    if ! curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --no-confirm; then
        error "Failed to install Nix"
        exit 1
    fi
    
    # Source the Nix environment
    if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
        source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    elif [[ -f ~/.nix-profile/etc/profile.d/nix.sh ]]; then
        source ~/.nix-profile/etc/profile.d/nix.sh
    fi
    
    success "✅ Nix installed successfully"
}

# Check prerequisites and install what's missing
check_prerequisites() {
    log "Checking prerequisites..."
    
    # Check if we're on a Linux system
    if [[ "$(uname -s)" != "Linux" ]]; then
        error "This script only works on Linux systems"
        exit 1
    fi
    
    # Check internet connectivity first
    if ! curl -sf --max-time 10 https://github.com &>/dev/null; then
        error "No internet connectivity or GitHub is unreachable"
        exit 1
    fi
    
    # Install Nix if not present
    if ! command -v nix &> /dev/null; then
        warn "Nix is not installed. Installing automatically..."
        install_nix
    else
        log "✅ Nix is already installed"
    fi
    
    # Verify Nix is working and has flakes
    if ! command -v nix &> /dev/null; then
        error "Nix installation failed or not in PATH"
        exit 1
    fi
    
    # Check if flakes are enabled
    if ! nix flake --help &> /dev/null 2>&1; then
        warn "Nix flakes not available. Enabling experimental features..."
        
        # Try to enable flakes
        if [[ -w /etc/nix/nix.conf ]]; then
            echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
        else
            # Create user config if we can't write to system config
            mkdir -p ~/.config/nix
            echo 'experimental-features = nix-command flakes' > ~/.config/nix/nix.conf
        fi
        
        # Restart nix daemon if available
        if systemctl is-active --quiet nix-daemon; then
            log "Restarting Nix daemon to apply flakes configuration..."
            sudo systemctl restart nix-daemon || true
        fi
        
        # Verify flakes work now
        if ! nix flake --help &> /dev/null 2>&1; then
            error "Failed to enable Nix flakes. Please restart your shell and try again."
            error "Or manually add 'experimental-features = nix-command flakes' to /etc/nix/nix.conf"
            exit 1
        fi
    fi
    
    success "✅ Prerequisites check passed"
}

# Determine target configuration
determine_config() {
    local arch="${1:-$(detect_architecture)}"
    local disk_layout="${2:-$(detect_disk_layout)}"
    
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

# Display detected configuration
show_detected_config() {
    local arch="${1:-$(detect_architecture)}"
    local disk_layout="${2:-$(detect_disk_layout)}"
    
    log "Detected architecture: $arch"
    log "Detected disk layout: $disk_layout"
}

# Validate flake configuration exists
validate_config() {
    local config="$1"
    
    log "Validating configuration: $config"
    
    # Use nix flake show without jq - just check if the config appears in output
    local flake_output
    if ! flake_output=$(nix flake show "${FLAKE_URL}" 2>/dev/null); then
        error "Failed to fetch flake information from ${FLAKE_URL}"
        exit 1
    fi
    
    # Check if our configuration exists in the nixosConfigurations section
    if ! echo "$flake_output" | grep -q "nixosConfigurations" || ! echo "$flake_output" | grep -q "$config"; then
        error "Configuration '$config' not found in flake"
        error "Available NixOS configurations:"
        echo "$flake_output" | grep -A 20 "nixosConfigurations" | grep -E "├───|└───" || echo "  None found"
        exit 1
    fi
    
    success "Configuration '$config' validated"
}

# Install NixOS using nixos-anywhere
install_nixos() {
    local config="$1"
    
    log "Starting NixOS installation with configuration: $config"
    
    # Detect if we're running locally on the target machine
    if [[ -z "${TARGET_HOST:-}" ]] || [[ "${TARGET_HOST}" == "root@localhost" ]] || [[ "${TARGET_HOST}" == "localhost" ]]; then
        log "Installing directly on this machine"
        
        # Use nixos-anywhere with kexec to install from within the target system
        log "Preparing for in-place NixOS installation using kexec..."
        
        # Install nixos-anywhere if not available
        if ! command -v nixos-anywhere &> /dev/null; then
            log "Installing nixos-anywhere..."
            nix profile install github:nix-community/nixos-anywhere
        fi
        
        log "This will:"
        log "  1. Load a NixOS installer into RAM using kexec"
        log "  2. Partition and format your disk"
        log "  3. Install NixOS with configuration: $config"
        log "  4. Automatically reboot into your new system"
        log ""
        warn "SSH connection WILL be lost during installation!"
        warn "The system will be available again in ~10-15 minutes"
        log ""
        
        local ip=$(hostname -I | awk '{print $1}')
        log "After installation, SSH back with: ssh gfanton@${ip}"
        log ""
        log "Starting installation in 5 seconds..."
        sleep 5
        
        # Run nixos-anywhere targeting localhost  
        # nixos-anywhere will handle the kexec process automatically
        nixos-anywhere \
            --flake "${FLAKE_URL}#${config}" \
            root@localhost
        
        # Note: The system will automatically reboot after successful installation
    else
        # Remote installation via nixos-anywhere
        local target_host="${TARGET_HOST}"
        log "Installing on remote host: $target_host"
        
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
    show_detected_config "$arch" "$disk_layout"
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
    
    # Read from /dev/tty to handle piped input (curl | bash)
    if [[ -t 0 ]]; then
        # Script run directly, stdin is available
        read -p "Are you sure you want to continue? (yes/no): " -r REPLY
    else
        # Script piped (curl | bash), read from terminal
        read -p "Are you sure you want to continue? (yes/no): " -r REPLY </dev/tty
    fi
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