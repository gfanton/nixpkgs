{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Enable Colima container runtime service
  services.colima = {
    enable = true;
    profile = "default";
    autoStart = true;
  };
}