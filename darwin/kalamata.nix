{ config, ... }:

{
  # Tailscale is the only way in (darwin/openssh.nix), so it runs as a launchd
  # daemon that starts at boot, with no user session needed. Enabling it also
  # drops the Tailscale app from the casks (darwin/homebrew.nix).
  services.tailscale.enable = true;

  # launchd starts tailscaled once at load and leaves it dead if it exits,
  # which on this host means unreachable until someone is at the console.
  launchd.daemons.tailscaled.serviceConfig.KeepAlive = true;

  # SSH keys stay in 1Password on the laptop, which forwards its agent here
  # (home/ssh-agent-forwarding.nix).
  users.primaryUser.sshAgent = "forwarded";

  users.users.${config.users.primaryUser.username}.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAIBARS+EkcOvC6Kw7kLq/Ui+Mz1HUMgjAIV8AaTR7Nm tzatziki"
  ];

  # Nobody is around to press the power button.
  power.restartAfterPowerFailure = true;
  power.restartAfterFreeze = true;
}
