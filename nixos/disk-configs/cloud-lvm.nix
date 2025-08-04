# LVM-based cloud disk configuration using disko
# More flexible partitioning with LVM for advanced use cases
{ lib, ... }:

{
  disko.devices = {
    disk.main = {
      device = lib.mkDefault "/dev/sda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          # BIOS boot partition for GRUB compatibility
          boot = {
            name = "boot";
            size = "1M";
            type = "EF02"; # BIOS boot partition
          };
          
          # EFI System Partition
          esp = {
            name = "ESP";
            size = "500M";
            type = "EF00"; # EFI System Partition
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "defaults" "umask=0077" ];
            };
          };
          
          # LVM Physical Volume
          root = {
            name = "root";
            size = "100%";
            content = {
              type = "lvm_pv";
              vg = "vg0";
            };
          };
        };
      };
    };
    
    # LVM Volume Group
    lvm_vg.vg0 = {
      type = "lvm_vg";
      lvs = {
        # Root logical volume (70% of space)
        root = {
          size = "70%FREE";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
            mountOptions = [
              "defaults"
              "noatime"
              "commit=60"
            ];
          };
        };
        
        # Home logical volume (20% of space)
        home = {
          size = "20%FREE";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/home";
            mountOptions = [
              "defaults"
              "noatime"
              "commit=60"
            ];
          };
        };
        
        # Swap logical volume (remaining space, ~10%)
        swap = {
          size = "100%FREE";
          content = {
            type = "swap";
            randomEncryption = false; # Cloud instances don't need swap encryption
          };
        };
      };
    };
  };
  
  # Swap configuration
  swapDevices = [
    { device = "/dev/vg0/swap"; }
  ];
}