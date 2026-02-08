# X299 configuration.nix
# Copy this to /etc/nixos/configuration.nix on X299

{ inputs, lib, config, pkgs, ... }:

{
  imports = [
    ./configuration-common-CORRECTED.nix
    ./X299-specific-CORRECTED.nix
  ];
}
