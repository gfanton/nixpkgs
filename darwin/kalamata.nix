{ config, ... }:

{
  services.tailscale.enable = true;
  launchd.daemons.tailscaled.serviceConfig.KeepAlive = true;

  users.primaryUser.sshAgent = "forwarded";

  users.users.${config.users.primaryUser.username}.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAIBARS+EkcOvC6Kw7kLq/Ui+Mz1HUMgjAIV8AaTR7Nm tzatziki"
  ];

  power.restartAfterPowerFailure = true;
  power.restartAfterFreeze = true;
}
