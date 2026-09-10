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
    cpu: 12

    # size of the disk in GiB to be allocated to the virtual machine.
    # Sparse-allocated, so it only consumes what the VM actually writes.
    # Colima can grow this on restart but never shrink it.
    disk: 200

    # size of the memory in GiB to be allocated to the virtual machine.
    memory: 16

    # the runtime to be used for the virtual machine (docker, containerd).
    runtime: docker
    ${
      # colima rejects `mountType: virtiofs` off macOS, and `vmType: vz` is
      # Apple's Virtualization.framework, so both are written on darwin only.
      lib.optionalString pkgs.stdenv.isDarwin ''

        # Resolved by colima rather than defaulted by it, so declaring them keeps
        # every start agreeing with the instance that already exists.
        vmType: vz
        mountType: virtiofs
      ''
    }
    # `disk` above is a separate lima volume; this is lima's own root disk.
    # Colima resolves it to 20 internally but only writes lima's `disk:` when it
    # is declared, otherwise emitting `0GiB`. Lima then reads 0 as a request to
    # shrink the existing 20GiB and refuses to start: "disk shrinking is not
    # supported". That breaks every restart while leaving creation intact.
    rootDisk: 20

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

    # The guest image is Ubuntu 24.04, whose GA kernel is 6.8. The HWE stack
    # backports 26.04's GA kernel to noble, which is both the newest kernel
    # 24.04 accepts and the only one above 6.8 still served by noble-updates:
    # the interim 6.11 and 6.14 HWE windows have already closed.
    #
    # Colima re-runs this on every start, hence the guard. The kernel it
    # installs boots on the NEXT start, so a freshly created VM reports the old
    # `uname -r` until it is restarted once.
    #
    # after-boot, not system: lima runs system scripts during boot, before DNS
    # resolves, so apt cannot reach ports.ubuntu.com and the install finds no
    # package. Lima logs that as a warning and carries on, so the start still
    # reports success. after-boot runs once the VM is up, and as the login user
    # rather than root, hence sudo.
    provision:
      - mode: after-boot
        script: |
          # Colima pipes after-boot scripts into `sh`, which is dash here, so a
          # shebang is ignored and `-o pipefail` is a syntax error on line 2.
          # POSIX only.
          set -eux
          if dpkg -s linux-image-virtual-hwe-24.04 >/dev/null 2>&1; then
            exit 0
          fi
          export DEBIAN_FRONTEND=noninteractive
          sudo -E apt-get update
          sudo -E apt-get install -y linux-image-virtual-hwe-24.04
  '';

in
{
  # Colima configuration for both macOS and Linux
  home.packages = [ pkgs.colima ];

  # Copied as a real writable file instead of a store symlink, because `colima
  # start` persists its resolved configuration by overwriting this path and a
  # failed write aborts the start. A rebuild re-applies the declarative content,
  # so nix stays source of truth.
  #
  # `colima delete` removes the whole profile directory including this file, so
  # a rebuild has to come between a delete and the next start.
  home.activation.colimaConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run install -Dm644 ${colima-config} \
      "${config.xdg.configHome}/colima/default/colima.yaml"
  '';

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
