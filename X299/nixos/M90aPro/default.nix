# /etc/nixos/M90aPro/default.nix
# Host-specific configuration for M90aPro (placeholder)
{ ... }:
{
  imports = [
    # ./hardware-configuration.nix  # Generate on target machine
    ./nvidia.nix
  ];

  networking.hostName = "M90aPro";
}
