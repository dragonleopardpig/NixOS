# X299-specific NixOS configuration
# This file contains settings unique to the X299 host

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
    ./nvidia.nix  # X299 has single NVIDIA GPU (no PRIME)
  ];

  # Hostname
  networking.hostName = "X299";

  # Nix settings
  nix.settings.trusted-users = [ "root" "thinky" ];

  # Disable swap
  swapDevices = lib.mkForce [ ];

  # Kernel parameters for swap disabled
  boot.kernelParams = [ "systemd.swap=0" ];

  # DDCCI driver for monitor control
  boot.extraModulePackages = [config.boot.kernelPackages.ddcci-driver];
  boot.kernelModules = ["i2c-dev" "ddcci_backlight"];
  services.udev.extraRules = ''
    KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
  '';
  hardware.i2c.enable = true;

  # GRUB theme resolution for this monitor
  boot.loader.grub2-theme.customResolution = "1920x1080";

  # X11 server configuration
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Display Manager: SDDM with Wayland
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    package = pkgs.kdePackages.sddm;
    extraPackages = with pkgs; [
      kdePackages.qtsvg
      kdePackages.qtmultimedia
      kdePackages.qtvirtualkeyboard
    ];
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

  # UWSM session manager
  programs.uwsm = {
    enable = true;
    waylandCompositors = {
      hyprland = {
        prettyName = "Hyprland";
        comment = "Hyprland compositor managed by UWSM";
        binPath = "/run/current-system/sw/bin/start-hyprland";
      };
    };
  };

  # Hyprland compositor
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    portalPackage = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
  };

  programs.dconf.enable = true;

  # Additional services
  systemd.user.services.orca.enable = false;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
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

  # Podman virtualization
  virtualisation.podman.enable = true;

  # X299-specific packages
  environment.systemPackages = with pkgs; [
    # Themes
    orchis-theme
    tela-icon-theme
    tela-circle-icon-theme
    fluent-icon-theme
    adwaita-icon-theme
    sddm-astronaut

    # Additional system tools
    brightnessctl
    wl-clipboard
    upower
    networkmanager
    power-profiles-daemon

    # Additional applications
    distrobox
    wofi
    rofi
    walker
    nemo-with-extensions

    # Sioyek wrapped to use XWayland (native Wayland has issues with NVIDIA)
    (pkgs.symlinkJoin {
      name = "sioyek-wrapped";
      paths = [ pkgs.sioyek ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/sioyek --set QT_QPA_PLATFORM xcb
      '';
    })
  ];

  # Environment variables for input method
  environment.variables.GTK_IM_MODULE = lib.mkForce "";
  environment.variables.QT_IM_MODULE = lib.mkForce "";

  # Use qt6Packages version of fcitx5-chinese-addons
  i18n.inputMethod.fcitx5.addons = with pkgs; [
    qt6Packages.fcitx5-chinese-addons
  ];

  # Fonts
  fonts.packages = with pkgs; [
    nerd-fonts.ubuntu
    nerd-fonts.ubuntu-sans
    nerd-fonts.ubuntu-mono
    nerd-fonts.caskaydia-cove
    noto-fonts
    noto-fonts-cjk-sans
  ];

  # System state version
  system.stateVersion = "25.11";
}
