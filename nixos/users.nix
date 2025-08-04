# User configuration for NixOS cloud instances
{ config, lib, pkgs, username, ... }:

{
  # Enable sudo for the wheel group
  security.sudo.wheelNeedsPassword = false;

  # User configuration
  users = {
    # Don't allow imperative user management
    mutableUsers = false;

    # Define users
    users = {
      # Main user from specialArgs
      ${username} = {
        isNormalUser = true;
        description = "gfanton";
        extraGroups = [ "wheel" ];
        shell = pkgs.zsh;
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDFmNJ0xGEEzdA/Dx7S4ySc1/LA1r2KqosH4SuKKsnca guilhem.fanton@gmail.com"
        ];
        hashedPassword = "!"; # Disabled password login, SSH only
      };

      # Keep root accessible during deployment
      root = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDFmNJ0xGEEzdA/Dx7S4ySc1/LA1r2KqosH4SuKKsnca guilhem.fanton@gmail.com"
        ];
      };
    };
  };

  # Enable ZSH system-wide
  programs.zsh.enable = true;

  # Note: networking is handled by systemd-networkd in hetzner-vm configuration
}
