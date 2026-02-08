# Common NixOS configuration shared between M90aPro and X299
# Based on actual current configurations (Feb 2026)
# Import host-specific configuration in configuration.nix

{ inputs, lib, config, pkgs, ... }:

let
  plymouthIcon = pkgs.callPackage ./custom_plymouth_logo.nix {};
in

{
  imports = [
    ./hardware-configuration.nix
    # NVIDIA config is host-specific - imported in host configs
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-users = [ "root" "thinky" ];

  # Bootloader configuration
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";

  # Kernel
  boot.kernelPackages = pkgs.linuxPackages_6_12;

  # GRUB theme
  boot.loader.grub2-theme = {
    enable = true;
    theme = "stylish";
    footer = true;
    # customResolution is host-specific
  };

  # Boot configuration
  boot = {
    consoleLogLevel = 3;
    initrd.verbose = false;
    initrd.systemd.enable = true;
    kernelParams = [
      "quiet"
      "splash"
      "intremap=on"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "rd.systemd.show_status=auto"
    ];

    # Plymouth boot splash
    plymouth.enable = true;
    plymouth.font = "${pkgs.hack-font}/share/fonts/truetype/Hack-Regular.ttf";
    plymouth.logo = "${plymouthIcon}/share/icons/hicolor/128x128/apps/nix-snowflake-rainbow.png";
  };

  # Networking
  networking.networkmanager.enable = true;

  # Timezone and locale
  time.timeZone = "Asia/Singapore";
  i18n.defaultLocale = "en_SG.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_SG.UTF-8";
    LC_IDENTIFICATION = "en_SG.UTF-8";
    LC_MEASUREMENT = "en_SG.UTF-8";
    LC_MONETARY = "en_SG.UTF-8";
    LC_NAME = "en_SG.UTF-8";
    LC_NUMERIC = "en_SG.UTF-8";
    LC_PAPER = "en_SG.UTF-8";
    LC_TELEPHONE = "en_SG.UTF-8";
    LC_TIME = "en_SG.UTF-8";
  };

  # X11 server
  services.xserver = {
    enable = true;
    xkb.layout = "us";
    xkb.variant = "";
  };

  # SDDM Display Manager (host-specific theme)
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    package = pkgs.kdePackages.sddm;
    extraPackages = with pkgs; [
      kdePackages.qtsvg
      kdePackages.qtmultimedia
      kdePackages.qtvirtualkeyboard
    ];
    # theme and settings are host-specific
  };

  # UWSM session manager
  programs.uwsm = {
    enable = true;
    waylandCompositors = {
      hyprland = {
        prettyName = "Hyprland";
        comment = "Hyprland compositor managed by UWSM";
        binPath = "/run/current-system/sw/bin/Hyprland";
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

  # Bluetooth
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  # Printing
  services.printing.enable = true;

  # Audio with PipeWire
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Sudo configuration
  security.sudo = {
    enable = true;
    extraRules = [
      {
        users = [ "thinky" ];
        commands = [
          {
            command = "ALL";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];
  };

  # User account
  users.users.thinky = {
    isNormalUser = true;
    description = "thinky";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

  # Firefox
  programs.firefox.enable = true;

  # Disable Orca
  systemd.user.services.orca.enable = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Common system packages
  environment.systemPackages = with pkgs; [
    claude-code
    fastfetch
    nnn

    # Archives
    zip
    xz
    unzip
    p7zip

    # Utils
    ripgrep
    jq
    yq-go
    eza
    fzf

    # Networking tools
    mtr
    iperf3
    dnsutils
    ldns
    aria2
    socat
    nmap
    ipcalc

    # Misc
    cowsay
    file
    which
    tree
    gnused
    gnutar
    gawk
    zstd
    gnupg

    # Nix related
    nix-output-monitor

    # Productivity
    hugo
    glow

    # Monitoring
    btop
    iotop
    iftop
    gpustat

    # System call monitoring
    strace
    ltrace
    lsof

    # System tools
    sysstat
    lm_sensors
    ethtool
    pciutils
    usbutils
    wget
    git
    remmina
    protonvpn-gui
    inetutils
    lshw
    gparted
    usbimager
    sassc
    redshift
    brightnessctl
    ddcutil
    wl-clipboard
    upower
    networkmanager
    power-profiles-daemon

    # Themes
    variety
    orchis-theme
    tela-icon-theme
    tela-circle-icon-theme
    fluent-icon-theme
    adwaita-icon-theme
    sddm-astronaut

    # Applications
    texliveFull
    onlyoffice-desktopeditors
    librecad
    freecad
    gimp
    inkscape
    pinta
    mission-center
    resources
    pandoc
    filezilla
    htop
    traceroute
    starship
    bat
    lsd
    imagemagick
    ffmpeg
    fim
    feh
    sxiv
    tiv
    chafa
    viu
    distrobox
    wofi
    rofi
    walker

    # Hyprland ecosystem
    hyprpaper
    waypaper
    hyprsunset
    hypridle
    hyprsysteminfo
    hyprshot
    satty
    slurp
    grim
    flameshot
    xdg-desktop-portal
    xdg-desktop-portal-hyprland

    # Development tools
    devenv
    direnv
    nix-direnv
    jsonrpc-glib
    git-credential-manager

    # Language servers
    nodejs
    yaml-language-server
    vscode-json-languageserver
    typescript-language-server
    bash-language-server
    systemd-language-server
    nginx-language-server
    kotlin-language-server
    perlnavigator
    nixd
    nix-index
    marksman
    gcc
    enchant
    pkg-config
    libxml2
    glib
    enchant2

    # Spell checking
    aspell
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers
    hunspell
    hunspellDicts.en_US

    # Sioyek wrapped for XWayland
    (pkgs.symlinkJoin {
      name = "sioyek-wrapped";
      paths = [ pkgs.sioyek ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/sioyek --set QT_QPA_PLATFORM xcb
      '';
    })

    # FHS environment
    (let base = pkgs.appimageTools.defaultFhsEnvArgs; in
     pkgs.buildFHSEnv (base // {
       name = "fhs";
       targetPkgs = pkgs:
         (base.targetPkgs pkgs) ++ (with pkgs; [
           pkg-config
           ncurses
         ]);
       profile = "export FHS=1";
       runScript = "bash";
       extraOutputsToInstall = ["dev"];
     }))
  ];

  # Podman virtualization
  virtualisation.podman.enable = true;

  # Default editor
  environment.variables.EDITOR = "xed";

  # Enable nix-ld
  programs.nix-ld.enable = true;

  # GPG agent
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Input method
  i18n.inputMethod = {
    type = "fcitx5";
    enable = true;
    fcitx5.addons = with pkgs; [
      fcitx5-gtk
      fcitx5-rime
      rime-data
      librime
      qt6Packages.fcitx5-chinese-addons
      fcitx5-nord
    ];
  };

  # Automatic garbage collection
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };

  # Common fonts
  fonts.packages = with pkgs; [
    nerd-fonts.ubuntu
    nerd-fonts.ubuntu-sans
    nerd-fonts.ubuntu-mono
    nerd-fonts.caskaydia-cove
    noto-fonts
    noto-fonts-cjk-sans
  ];

  # State version
  system.stateVersion = "25.11";
}
