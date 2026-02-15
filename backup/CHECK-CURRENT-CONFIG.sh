#!/usr/bin/env bash
# Check the ACTUAL current NixOS configuration on M90aPro

echo "=== Current /etc/nixos/hardware-configuration.nix ==="
cat /etc/nixos/hardware-configuration.nix

echo ""
echo "=== Current /etc/nixos/configuration.nix LUKS lines ==="
grep -A2 -B2 "luks" /etc/nixos/configuration.nix
