# Basic cloud disk configuration using disko
# Simple GPT partitioning suitable for most cloud instances
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
          
          # EFI System Partition for UEFI systems
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
          
          # Root partition using all remaining space
          root = {
            name = "root";  
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
              mountOptions = [ 
                "defaults"
                "noatime"     # Performance optimization
                "commit=60"   # Reduce write frequency
              ];
            };
          };
        };
      };
    };
  };
  
  # Filesystem configurations are handled by disko
}