# /etc/nixos/X299/default.nix
# Host-specific configuration for X299 desktop
{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./nvidia.nix
  ];

  networking.hostName = "X299";
}
