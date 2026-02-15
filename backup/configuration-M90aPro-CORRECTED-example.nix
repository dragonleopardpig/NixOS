# M90aPro configuration.nix
# Copy this to /etc/nixos/configuration.nix on M90aPro

{ inputs, lib, config, pkgs, ... }:

{
  imports = [
    ./configuration-common-CORRECTED.nix
    ./M90aPro-specific-CORRECTED.nix
  ];
}
