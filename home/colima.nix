{
  config,
  lib,
  pkgs,
  ...
}:

let
  colima-config = pkgs.writeText "colima.yaml" ''
    # Colima configuration
    # See: https://github.com/abiosoft/colima/blob/main/docs/FAQ.md#how-can-i-customize-colima-configuration

    # number of CPUs to be allocated to the virtual machine.
    cpu: 4

    # size of the disk in GiB to be allocated to the virtual machine.
    disk: 60

    # size of the memory in GiB to be allocated to the virtual machine.
    memory: 8

    # the runtime to be used for the virtual machine (docker, containerd).
    runtime: docker

    # architecture of the virtual machine (x86_64, aarch64).  
    # Default is the architecture of the host machine.
    # arch: aarch64

    # kubernetes configuration for colima virtual machine.
    kubernetes:
      enabled: false
      # version: v1.27.1
      # k3sArgs: []

    # docker daemon configuration that maps directly to daemon.json.
    # https://docs.docker.com/engine/reference/commandline/dockerd/#daemon-configuration-file.
    # NOTE: some keys may not be supported on macOS.
    docker: {}

    # containerd configuration that maps directly to config.toml.
    # https://github.com/containerd/containerd/blob/main/docs/man/containerd-config.toml.5.md
    # NOTE: some keys may not be supported on macOS.
    containerd: {}

    # file system mounts for the virtual machine. Declared explicitly because
    # colima serializes an absent key as `mounts: null` in the instance config,
    # which suppresses the default $HOME mount and leaves bind mounts empty.
    mounts:
      - location: "~"
        writable: true

    # virtual machine configuration
    vm:
      # autoStart configures the virtual machine to automatically start on login.
      autoStart: true
  '';

in
{
  # Colima configuration for both macOS and Linux
  home.packages = [ pkgs.colima ];

  # Add Colima configuration file to XDG config directory
  home.file."${config.xdg.configHome}/colima/default/colima.yaml" = {
    source = colima-config;
  };

  # COLIMA_HOME is the only directory variable colima honors (it keeps config
  # and VM data together, with no config/data split). Without it, colima
  # prefers legacy ~/.colima whenever that directory exists.
  home.sessionVariables = {
    COLIMA_HOME = "${config.xdg.configHome}/colima";
  };

  # Create systemd user service for Colima on Linux
  systemd.user.services.colima = lib.mkIf pkgs.stdenv.isLinux {
    Unit = {
      Description = "Colima container runtime";
      After = [ "graphical-session.target" ];
      Wants = [ "graphical-session.target" ];
    };

    Service = {
      Type = "forking";
      # Use --save-config=false to prevent modifying the read-only config file
      ExecStart = "${pkgs.colima}/bin/colima start --profile default --save-config=false";
      ExecStop = "${pkgs.colima}/bin/colima stop";
      Restart = "on-failure";
      RestartSec = 5;
      Environment = [
        "COLIMA_HOME=${config.xdg.configHome}/colima"
      ];
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  # Enable the service on Linux
  systemd.user.startServices = lib.mkIf pkgs.stdenv.isLinux true;
}
