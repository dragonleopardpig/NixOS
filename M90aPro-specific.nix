# M90aPro-specific NixOS configuration
# This file contains settings unique to the M90aPro host

{ inputs, lib, config, pkgs, ... }:

{
  imports = [
    ./nvidia.nix  # M90aPro has dual GPU (Intel + NVIDIA) with PRIME
  ];

  # Hostname
  networking.hostName = "M90aPro";

  # Note: LUKS encryption is already configured in hardware-configuration.nix
  # No need to duplicate it here

  # Console mode for systemd-boot
  boot.loader.systemd-boot.consoleMode = "max";

  # GRUB theme resolution for this monitor
  boot.loader.grub2-theme.customResolution = "2560x1440";

  # Desktop Environment: LightDM + Cinnamon
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.background = ./assets/Nixos_2560x1440.jpg;
  services.xserver.desktopManager.cinnamon.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # M90aPro-specific packages
  environment.systemPackages = with pkgs; [
    nerd-fonts.ubuntu
    nerd-fonts.ubuntu-sans
    nerd-fonts.ubuntu-mono
    noto-fonts
    noto-fonts-extra
    noto-fonts-cjk-sans
  ];

  # Use fcitx5-chinese-addons (not qt6Packages version)
  i18n.inputMethod.fcitx5.addons = with pkgs; [
    fcitx5-chinese-addons
  ];

  # System state version
  system.stateVersion = "25.05";
}
