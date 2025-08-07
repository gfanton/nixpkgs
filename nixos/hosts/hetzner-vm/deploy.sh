#!/usr/bin/env bash
# Hetzner Cloud NixOS deployment script
# Based on: https://github.com/LGUG2Z/nixos-hetzner-cloud-starter
# Simplified deployment using nixos-anywhere

set -euo pipefail

# Configuration
REMOTE_HOST="${1:-}"
FLAKE_CONFIG="${2:-.#hetzner-vm}"  # Default to local flake

if [ -z "$REMOTE_HOST" ]; then
    echo "Usage: $0 <remote-host> [flake-ref]"
    echo "Example: $0 root@159.69.46.1"
    echo "Example: $0 root@159.69.46.1 github:gfanton/nixpkgs/feat/nixos#hetzner-vm"
    exit 1
fi

echo "🚀 Deploying NixOS to $REMOTE_HOST"
echo "📦 Using flake: $FLAKE_CONFIG"

# Deploy with nixos-anywhere
echo "⚙️  Running nixos-anywhere..."
nix run github:numtide/nixos-anywhere -- --flake "$FLAKE_CONFIG" "$REMOTE_HOST"

echo ""
echo "✅ Deployment completed!"
echo ""
echo "📝 Important: The server IP will be removed from known_hosts"
echo "   You may see a warning about host identification changing."
echo "   This is expected. Remove the old entry with:"
echo "   ssh-keygen -f ~/.ssh/known_hosts -R \"${REMOTE_HOST#root@}\""
echo ""
echo "🔄 After removing the old host key, you can SSH with:"
echo "   ssh gfanton@${REMOTE_HOST#root@}"
echo ""
