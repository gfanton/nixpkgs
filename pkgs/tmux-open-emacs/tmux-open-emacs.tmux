#!/usr/bin/env bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPTS_DIR="$CURRENT_DIR/scripts"

# Fallback to absolute path if running from Nix store
if [[ ! -x "$SCRIPTS_DIR/toe" && "$CURRENT_DIR" == /nix/store/* ]]; then
    SCRIPTS_DIR="$CURRENT_DIR/scripts"
fi

default_key_bindings_open="C-e"

# tmux show-option -gqv '@toe-open-key' ⇒ return the global option value or empty string
get_tmux_option() {
    local option="$1"
    local default_value="$2"
    local option_value="$(tmux show-option -gqv "$option")"
    if [ -z "$option_value" ]; then
        echo "$default_value"
    else
        echo "$option_value"
    fi
}

# Set key bindings
set_key_bindings() {
    local key_open
    key_open=$(get_tmux_option '@toe-open-key' "$default_key_bindings_open")
    
    # Use absolute path to the script
    if [[ -x "$SCRIPTS_DIR/toe" ]]; then
        tmux bind-key "$key_open" run-shell "$SCRIPTS_DIR/toe"
    else
        tmux display-message "Error: toe script not found at $SCRIPTS_DIR/toe"
    fi
}

main() {
    set_key_bindings
}

main