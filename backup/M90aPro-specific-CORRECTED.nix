# M90aPro-specific NixOS configuration
# Based on actual current /etc/nixos/configuration.nix

{ inputs, lib, config, pkgs, ... }:

{
  imports = [
    ./M90aPro/hardware-configuration.nix
    ./M90aPro/nvidia_offload.nix  # M90aPro uses nvidia_offload for dual GPU
  ];

  # Hostname
  networking.hostName = "M90aPro";
  
  # GRUB resolution for 2560x1440 display
  boot.loader.grub2-theme.customResolution = "2560x1440";
}
