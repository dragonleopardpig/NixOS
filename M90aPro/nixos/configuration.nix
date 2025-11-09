# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ inputs, lib, config,  pkgs, ... }:

let
  plymouthIcon = pkgs.callPackage ./custom_plymouth_logo.nix {};
  sddm-astronaut = pkgs.sddm-astronaut.override {
    embeddedTheme = "cyberpunk";
    # themeConfig = {
    #   AccentColor = "#746385";
    #   FormPosition = "left";
    #
    #   ForceHideCompletePassword = true;
    # };
  };
in

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./nvidia.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Don't delete
  boot.initrd.luks.devices."luks-51c7cb8c-d514-40e1-8286-0185987e196c".device = "/dev/disk/by-uuid/51c7cb8c-d514-40e1-8286-0185987e196c";
  
  # Bootloader.
    boot.loader.systemd-boot.enable = false;
    boot.loader.grub.enable = true;
    boot.loader.grub.device = "nodev";
    boot.loader.grub.useOSProber = true;
    boot.loader.grub.efiSupport = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot";
    boot.loader.systemd-boot.consoleMode = "max";

    # Use latest kernel.
    # boot.kernelPackages = pkgs.linuxPackages_latest;
    boot.kernelPackages = pkgs.linuxPackages_6_12;
    
    boot.loader.grub2-theme = {
      enable = true;
      theme = "stylish";
      footer = true;
      customResolution = "2560x1440";  # Optional: Set a custom resolution
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
     
  networking.hostName = "M90aPro"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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
  services.xserver = {
    enable = true;
    xkb.layout = "us";
    xkb.variant = "";
    desktopManager.cinnamon.enable = true;
  };
  
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    package = pkgs.kdePackages.sddm;
    extraPackages = with pkgs; [
      kdePackages.qtsvg
      kdePackages.qtmultimedia
      kdePackages.qtvirtualkeyboard
    ];
    theme = "sddm-astronaut-theme"; # Or "sddm-astronaut" if that's the package name
    settings = {
      Theme = {
        Current = "sddm-astronaut-theme"; # Or "sddm-astronaut"
        # This line overrides the embedded theme with the one you want
        # You can find available themes in /run/current-system/sw/share/sddm/themes/sddm-astronaut-theme/Themes/
        # embeddedTheme = "cyberpunk"; 
      };
    };
  };
  
  # environment.sessionVariables.NIXOS_OZONE_WL = "1"; # This variable fixes electron apps in wayland
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
  
  programs.hyprland = {
    enable = true;
    withUWSM = true; # recommended for most users
    xwayland.enable = true; # Xwayland can be disabled.
  };
  
  # Enable the Cinnamon Desktop Environment.
  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.displayManager.lightdm.background = ./assets/Nixos_2560x1440.jpg;
  # services.xserver.desktopManager.gnome.enable = true;
  # services.xserver.desktopManager.cinnamon.enable = true;

  programs.dconf.enable = true;
  
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
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  security.sudo = {
    enable = true;
    extraRules = [
      {
        # Replace "yourusername" with the actual username
        users = [ "thinky" ]; 
        commands = [
          {
            command = "ALL"; # Allows all commands
            options = [ "NOPASSWD" ];
          }
        ];
      }
      # You can add more rules here for other users or specific commands
    ];
  };
  
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.thinky = {
    isNormalUser = true;
    description = "thinky";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
    #  thunderbird
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
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
    eza # A modern replacement for ‘ls’
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
    #
    # it provides the command `nom` works just like `nix`
    # with more details log output
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
    emacs-gtk
    gparted
    usbimager
    sassc
    redshift

    # themes
    variety
    orchis-theme
    tela-icon-theme
    tela-circle-icon-theme
    catppuccin
    fluent-icon-theme
    epapirus-icon-theme
    catppuccin-fcitx5
    catppuccin-grub
    catppuccin-sddm
    adwaita-icon-theme
    sddm-astronaut

    # others
    git-credential-manager # type "unset SSH_ASKPASS" in command prompt
    ddcutil
    nerd-fonts.ubuntu
    nerd-fonts.ubuntu-sans
    nerd-fonts.ubuntu-mono
    noto-fonts
    noto-fonts-extra
    noto-fonts-cjk-sans
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
    fzf
    fim
    feh
    sxiv
    tiv
    chafa
    viu
    distrobox
    wofi
    rofi
    ashell
    adwaita-icon-theme 
    inputs.nix-software-center.packages.${system}.nix-software-center
    inputs.nixos-conf-editor.packages.${system}.nixos-conf-editor
    (python3.withPackages (python-pkgs: with python-pkgs; [
      pandas
      requests
      scipy
      sympy
      scikit-learn
      scikit-image
      jupyterlab
      numpy
      matplotlib
      python-lsp-server
      pyright
      emacsPackages.lsp-pyright
      emacsPackages.jsonrpc
      python-lsp-jsonrpc
      python-jsonrpc-server
      jsonrpclib-pelix
      jsonrpc-websocket
      jsonrpc-base
      jsonrpc-async
      ajsonrpc
      jsonrpc-glib
      ipykernel
      jupyter
      pyzmq
      emacsPackages.zmq
    ]))
    # Create an FHS environment using the command `fhs`, 
    #enabling the execution of non-NixOS packages in NixOS!
    (let base = pkgs.appimageTools.defaultFhsEnvArgs; in
      pkgs.buildFHSEnv (base // {
      name = "fhs";
      targetPkgs = pkgs:
        # pkgs.buildFHSEnv provides only a minimal FHS environment,
        # lacking many basic packages needed by most software.
        # Therefore, we need to add them manually.
        #
        # pkgs.appimageTools provides basic packages required by most software.
        (base.targetPkgs pkgs) ++ (with pkgs; [
          pkg-config
          ncurses
          # Feel free to add more packages here if needed.
        ]
      );
      profile = "export FHS=1";
      runScript = "bash";
      extraOutputsToInstall = ["dev"];
    }))
  ];
  
  virtualisation.podman.enable = true;

  # Set the default editor to vim
  environment.variables.EDITOR = "xed";
  
  # Optional: Enable nix-ld for automatic handling of dynamic libraries
  # This is often recommended for seamless integration with non-Nix software.
  programs.nix-ld.enable = true;
  
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.bash = {
    shellAliases = {
      ls = "eza --icons=always --group-directories-first --sort=extension";
      gc = "git commit -m";
      rebuild = "sudo nixos-rebuild switch";
    };
   interactiveShellInit = ''
          ${pkgs.fastfetch}/bin/fastfetch
        '';
   promptInit = ''
    if [ "$TERM" != "dumb" ] || [ -n "$INSIDE_EMACS" ]; then
     PROMPT_COLOR="1;31m"
     ((UID)) && PROMPT_COLOR="1;32m"
     BOLD="\\[\\e[1m\\]"
     GOLD="\\[\\e[38;5;220m\\]"
     GREEN="\\[\\e[0;1;38;5;154m\\]"
     PURPLE="\\[\\e[1;35m\\]"
     RED="\\[\\e[0;1;38;5;160m\\]"
     ORANGE="\\[\\e[0;1;38;5;208m\\]"
     BLUE="\\[\\e[38;5;153m\\]"
     CYAN="\\[\\e[36m\\]"
     RESET="\\[\\e[0m\\]"
     if [ -n "$INSIDE_EMACS" ]; then
         # Emacs term mode doesn't support xterm title escape sequence (\e]0;)
         PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
     else
PS1="\n\[\033[$PROMPT_COLOR\][$BOLD$BLUE\d $BOLD$CYAN\t $BOLD$GREEN\u$BOLD$PURPLE@$BOLD$ORANGE\h$BOLD$RED:$BOLD$GOLD\w\[\033[$PROMPT_COLOR\]]\n$BOLD$BLUE\$\[\033[0m\] "
    fi
    if test "$TERM" = "xterm"; then
      PS1="\[\033]2;\h:\u:\w\007\]$PS1"
    fi
    fi
    '';
  };

  i18n.inputMethod = {
    type = "fcitx5";
    enable = true;
    fcitx5.addons = with pkgs; [
      fcitx5-gtk   # Or fcitx5-qt for KDE Plasma
      fcitx5-rime
      rime-data
      librime
      fcitx5-chinese-addons
      fcitx5-nord  # a color theme
    ];
  };
    
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
