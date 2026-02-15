# /etc/nixos/X299/disko.nix
# Declarative disk layout for future reinstalls.
# This is REFERENCE ONLY - do NOT run disko on the live system.
#
# Target layout on /dev/sda (953.9G SATA):
#   sda1 (1G)    - ESP /boot, vfat
#   sda2 (~870G) - LUKS2 -> ext4 / (root)
#   sda3 (~83G)  - LUKS2 -> ext4 /home
#   No swap partition
{ ... }:
{
  disko.devices = {
    disk.main = {
      type = "disk";
      device = "/dev/sda";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            size = "1G";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "umask=0077" ];
            };
          };
          root = {
            size = "870G";
            content = {
              type = "luks";
              name = "cryptroot";
              settings.allowDiscards = true;
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
          home = {
            size = "100%";
            content = {
              type = "luks";
              name = "crypthome";
              settings.allowDiscards = true;
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/home";
              };
            };
          };
        };
      };
    };
  };
}
