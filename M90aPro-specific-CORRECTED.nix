# M90aPro-specific NixOS configuration
# Based on actual current /etc/nixos/configuration.nix

{ inputs, lib, config, pkgs, ... }:

let
  sddm-astronaut = pkgs.sddm-astronaut.override {
    embeddedTheme = "pixel_sakura";
    themeConfig = {
      FormPosition = "left";
    };
  };
in

{
  imports = [
    ./nvidia_offload.nix  # M90aPro uses nvidia_offload for dual GPU
  ];

  # Hostname
  networking.hostName = "M90aPro";

  # Boot console mode
  boot.loader.systemd-boot.consoleMode = "max";

  # NVIDIA in initrd
  boot.initrd.kernelModules = ["nvidia"];
  boot.extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];

  # GRUB resolution for 2560x1440 display
  boot.loader.grub2-theme.customResolution = "2560x1440";

  # Console font configuration
  console.font = "${pkgs.terminus_font}/share/consolefonts/ter-i20n.psf.gz";
  console.packages = with pkgs; [ terminus_font ];

  # SDDM theme (different from X299: pixel_sakura vs cyberpunk)
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

  # Additional services (following X299)
  services.upower.enable = true;
  services.gnome.gnome-keyring.enable = true;

  # Blueman service (M90aPro specific - laptop needs this)
  services.blueman.enable = true;

  # Detailed Bluetooth settings (M90aPro specific)
  hardware.bluetooth.settings = {
    General = {
      Experimental = true;
      FastConnectable = true;
    };
    Policy = {
      AutoEnable = true;
    };
  };

  # User configuration with additional groups (following X299)
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

  # Input method environment variables (following X299)
  environment.variables.GTK_IM_MODULE = lib.mkForce "";
  environment.variables.QT_IM_MODULE = lib.mkForce "";

  # M90aPro-specific packages
  environment.systemPackages = with pkgs; [
    # Emacs pgtk version (following X299 - Wayland-native)
    ((emacsPackagesFor emacs-pgtk).emacsWithPackages (
      epkgs: with epkgs; [
        vterm
        direnv
        lsp-pyright
        zmq
      ]
    ))

    # Additional M90aPro packages
    mesa-demos
    efibootmgr
    gptfdisk
    util-linux
    lua
    nemo-with-extensions
    claude-monitor
  ];
}
