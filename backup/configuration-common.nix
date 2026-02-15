# Common NixOS configuration shared between M90aPro and X299
# Import host-specific configuration:
#   imports = [ ./M90aPro.nix ];
# or
#   imports = [ ./X299.nix ];

{ inputs, lib, config, pkgs, ... }:

let
  plymouthIcon = pkgs.callPackage ./custom_plymouth_logo.nix {};
in

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # nvidia.nix is host-specific - imported in M90aPro-specific.nix or X299-specific.nix
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader configuration
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.efiSupport = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";

  # Use kernel 6.12
  boot.kernelPackages = pkgs.linuxPackages_6_12;

  boot.loader.grub2-theme = {
    enable = true;
    theme = "stylish";
    footer = true;
    # customResolution is host-specific - set in host config
  };

  boot = {
    # silence first boot output
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

    # plymouth, showing after LUKS unlock
    plymouth.enable = true;
    plymouth.font = "${pkgs.hack-font}/share/fonts/truetype/Hack-Regular.ttf";
    plymouth.logo = "${plymouthIcon}/share/icons/hicolor/128x128/apps/nix-snowflake-rainbow.png";
  };

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Singapore";

  # Select internationalisation properties.
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

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
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

  # Define a user account. Don't forget to set a password with 'passwd'.
  users.users.thinky = {
    isNormalUser = true;
    description = "thinky";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Common system packages
  environment.systemPackages = with pkgs; [
    fastfetch
    nnn # terminal file manager

    # archives
    zip
    xz
    unzip
    p7zip

    # utils
    ripgrep # recursively searches directories for a regex pattern
    jq # A lightweight and flexible command-line JSON processor
    yq-go # yaml processor https://github.com/mikefarah/yq
    eza # A modern replacement for 'ls'
    fzf # A command-line fuzzy finder

    # networking tools
    mtr # A network diagnostic tool
    iperf3
    dnsutils  # `dig` + `nslookup`
    ldns # replacement of `dig`, it provide the command `drill`
    aria2 # A lightweight multi-protocol & multi-source command-line download utility
    socat # replacement of openbsd-netcat
    nmap # A utility for network discovery and security auditing
    ipcalc  # it is a calculator for the IPv4/v6 addresses

    # misc
    cowsay
    file
    which
    tree
    gnused
    gnutar
    gawk
    zstd
    gnupg

    # nix related
    nix-output-monitor

    # productivity
    hugo # static site generator
    glow # markdown previewer in terminal

    btop  # replacement of htop/nmon
    iotop # io monitoring
    iftop # network monitoring

    # system call monitoring
    strace # system call monitoring
    ltrace # library call monitoring
    lsof # list open files

    # system tools
    sysstat
    lm_sensors # for `sensors` command
    ethtool
    pciutils # lspci
    usbutils # lsusb
    wget
    git # git config --global core.askpass ""
    remmina
    protonvpn-gui
    inetutils
    lshw
    gparted
    usbimager
    sassc
    redshift
    variety
    git-credential-manager # type "unset SSH_ASKPASS" in command prompt
    ddcutil
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

    # Hyprland ecosystem (common for both hosts now)
    gpustat
    hyprpaper
    swww
    hyprsunset
    hypridle
    hyprsysteminfo
    hyprshot
    waypaper
    satty
    slurp
    grim
    flameshot
    xdg-desktop-portal
    xdg-desktop-portal-hyprland

    # Development tools (common for both hosts now)
    jsonrpc-glib
    devenv
    direnv
    nix-direnv
    claude-code
    claude-monitor

    # Emacs with packages (emacs-pgtk for Wayland support)
    ((emacsPackagesFor emacs-pgtk).emacsWithPackages (
      epkgs: with epkgs; [
        vterm
        direnv
        lsp-pyright
        zmq
      ]
    ))

    # Spell checking
    aspell
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers

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
    hunspell
    hunspellDicts.en_US

    # Create an FHS environment using the command `fhs`,
    #enabling the execution of non-NixOS packages in NixOS!
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

  # Set the default editor
  environment.variables.EDITOR = "xed";

  # Optional: Enable nix-ld for automatic handling of dynamic libraries
  programs.nix-ld.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Input method configuration
  i18n.inputMethod = {
    type = "fcitx5";
    enable = true;
    fcitx5.addons = with pkgs; [
      fcitx5-gtk
      fcitx5-rime
      rime-data
      librime
      fcitx5-nord  # a color theme
    ];
  };

  # Automatic garbage collection
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };
}
