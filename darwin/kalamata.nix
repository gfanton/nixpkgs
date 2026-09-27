{ config, ... }:

let
  sshKeys = import ../lib/ssh-keys.nix;
in
{
  services.tailscale.enable = true;
  launchd.daemons.tailscaled.serviceConfig.KeepAlive = true;

  users.primaryUser.sshAgent = "forwarded";
  users.primaryUser.sshKey = sshKeys.kalamata;

  users.users.${config.users.primaryUser.username}.openssh.authorizedKeys.keys = [ sshKeys.tzatziki ];

  power.restartAfterPowerFailure = true;
  power.restartAfterFreeze = true;
}
