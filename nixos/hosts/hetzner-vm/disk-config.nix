# Disk configuration for Hetzner Cloud VMs using disko
# Based on: https://github.com/LGUG2Z/nixos-hetzner-cloud-starter
# Simplified with LVM for better reliability and flexibility

{ lib, ... }:

{
  disko.devices = {
    disk.disk1 = {
      device = lib.mkDefault "/dev/sda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          # BIOS boot partition for legacy compatibility
          boot = {
            name = "boot";
            size = "1M";
            type = "EF02";
          };
          # EFI System Partition for UEFI boot
          esp = {
            name = "ESP";
            size = "500M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          # Main partition using LVM for flexibility
          root = {
            name = "root";
            size = "100%";
            content = {
              type = "lvm_pv";
              vg = "pool";
            };
          };
        };
      };
    };
    # LVM Volume Group configuration
    lvm_vg = {
      pool = {
        type = "lvm_vg";
        lvs = {
          # Root logical volume using all available space
          root = {
            size = "100%FREE";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
              mountOptions = [
                "defaults"
                "noatime"
                "discard"  # Enable TRIM for SSDs
              ];
            };
          };
        };
      };
    };
  };
}