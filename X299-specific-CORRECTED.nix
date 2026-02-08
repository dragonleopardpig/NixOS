# X299-specific NixOS configuration
# Based on actual ~/Downloads/NixOS/X299/nixos/configuration.nix

{ inputs, lib, config, pkgs, ... }:

let
  sddm-astronaut = pkgs.sddm-astronaut.override {
    embeddedTheme = "cyberpunk";
    themeConfig = {
      FormPosition = "left";
    };
  };
in

{
  imports = [
    ./nvidia.nix  # X299 uses nvidia.nix for single GPU
  ];

  # Hostname
  networking.hostName = "X299";

  # Disable swap completely
  swapDevices = lib.mkForce [ ];
  boot.kernelParams = [ "systemd.swap=0" ];

  # DDCCI driver for monitor brightness control
  boot.extraModulePackages = [config.boot.kernelPackages.ddcci-driver];
  boot.kernelModules = ["i2c-dev" "ddcci_backlight"];
  services.udev.extraRules = ''
    KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
  '';
  hardware.i2c.enable = true;

  # GRUB resolution for 1920x1080 display
  boot.loader.grub2-theme.customResolution = "1920x1080";

  # SDDM theme
  services.displayManager.sddm = {
    theme = "sddm-astronaut-theme";
    settings = {
      General = {
        DefaultSession = "hyprland-uwsm.desktop";
      };
      Theme = {
        Current = "sddm-astronaut-theme";
      };
    };
  };

  # Additional services
  services.upower.enable = true;
  services.gnome.gnome-keyring.enable = true;

  # User configuration with additional groups
  users.users.thinky.extraGroups = [ "i2c" "podman" ];
  users.users.thinky.subGidRanges = [
    {
      count = 65536;
      startGid = 1000;
    }
  ];
  users.users.thinky.subUidRanges = [
    {
      count = 65536;
      startUid = 1000;
    }
  ];

  # Input method environment variables
  environment.variables.GTK_IM_MODULE = lib.mkForce "";
  environment.variables.QT_IM_MODULE = lib.mkForce "";

  # X299-specific packages
  environment.systemPackages = with pkgs; [
    # Emacs pgtk version (Wayland-native)
    ((emacsPackagesFor emacs-pgtk).emacsWithPackages (
      epkgs: with epkgs; [
        vterm
        direnv
        lsp-pyright
        zmq
      ]
    ))

    # Additional X299 packages
    nemo-with-extensions
    claude-monitor
  ];
}
