# Configuration Comparison: M90aPro vs X299

## Summary Table

| Feature | M90aPro | X299 | Location |
|---------|---------|------|----------|
| **Hostname** | M90aPro | X299 | Host-specific |
| **Desktop Environment** | Cinnamon | Hyprland | Host-specific |
| **Display Manager** | LightDM | SDDM (Wayland) | Host-specific |
| **Resolution** | 2560x1440 | 1920x1080 | Host-specific |
| **LUKS Encryption** | Yes | No | Host-specific |
| **Swap** | Enabled | Disabled | Host-specific |
| **DDCCI Driver** | No | Yes | Host-specific |
| **Emacs** | emacs-gtk | emacs-pgtk | Host-specific |
| **Virtualization** | None | Podman | Host-specific |
| **Bash Prompt** | Custom colorful | Default | Host-specific |
| **Python/Jupyter** | Yes | No | Host-specific |
| **Dev Tools** | Minimal | Extensive (LSP, direnv, claude-code) | Host-specific |
| **Nix GC** | Manual | Automatic daily | Host-specific |
| **Kernel** | 6.12 | 6.12 | Common |
| **Bootloader** | GRUB | GRUB | Common |
| **Plymouth** | Yes | Yes | Common |
| **Timezone** | Asia/Singapore | Asia/Singapore | Common |
| **Audio** | PipeWire | PipeWire | Common |
| **Input Method** | fcitx5 | fcitx5 | Common (with variations) |

## Detailed Breakdown

### Boot Configuration

#### Common:
- GRUB bootloader with EFI support
- OS Prober enabled
- Kernel 6.12
- Plymouth boot splash
- Quiet boot with kernel parameters

#### M90aPro Only:
```nix
boot.initrd.luks.devices."luks-51c7cb8c-d514-40e1-8286-0185987e196c"
boot.loader.systemd-boot.consoleMode = "max"
boot.loader.grub2-theme.customResolution = "2560x1440"
```

#### X299 Only:
```nix
swapDevices = lib.mkForce [ ]
boot.kernelParams = [ "systemd.swap=0" ]
boot.extraModulePackages = [ ddcci-driver ]
boot.kernelModules = ["i2c-dev" "ddcci_backlight"]
boot.loader.grub2-theme.customResolution = "1920x1080"
nix.settings.trusted-users = [ "root" "thinky" ]
```

### Desktop Environment

#### M90aPro (Traditional X11):
```nix
services.xserver.displayManager.lightdm.enable = true
services.xserver.displayManager.lightdm.background = ./assets/Nixos_2560x1440.jpg
services.xserver.desktopManager.cinnamon.enable = true
```

#### X299 (Modern Wayland):
```nix
services.displayManager.sddm = {
  enable = true
  wayland.enable = true
  theme = "sddm-astronaut-theme"
}
programs.uwsm.enable = true
programs.hyprland = {
  enable = true
  withUWSM = true
  xwayland.enable = true
}
```

### Package Differences

#### M90aPro Exclusive Packages:
- emacs-gtk (X11 version)
- nix-software-center
- nixos-conf-editor
- Python ecosystem:
  - jupyterlab, jupyter
  - pandas, numpy, matplotlib
  - scipy, sympy
  - scikit-learn, scikit-image
- noto-fonts-extra

#### X299 Exclusive Packages:
- emacs-pgtk (Wayland version)
- **Hyprland ecosystem**:
  - hyprpaper, swww
  - hyprsunset, hypridle, hyprsysteminfo, hyprshot
  - waypaper, satty, slurp, grim
- **Development tools**:
  - devenv, direnv, nix-direnv
  - claude-code, claude-monitor
  - Language servers: yaml, json, typescript, bash, systemd, nginx, kotlin, perl, nixd
  - nix-index, marksman
- **Desktop apps**:
  - distrobox
  - wofi, rofi, walker
  - nemo-with-extensions
- **Wayland utilities**:
  - wl-clipboard
  - brightnessctl
- **Themes**:
  - orchis-theme
  - tela-icon-theme, tela-circle-icon-theme
  - fluent-icon-theme
  - sddm-astronaut
- **Spell checking**:
  - aspell with dictionaries
  - hunspell with en_US
- **Wrapped packages**:
  - sioyek (XWayland wrapper)

### User Configuration

#### M90aPro:
```nix
users.users.thinky = {
  extraGroups = [ "networkmanager" "wheel" ]
}
```

#### X299:
```nix
users.users.thinky = {
  extraGroups = [ "networkmanager" "wheel" "i2c" "podman" ]
  subGidRanges = [ { count = 65536; startGid = 1000; } ]
  subUidRanges = [ { count = 65536; startUid = 1000; } ]
}
```

### Input Method

#### M90aPro:
```nix
i18n.inputMethod.fcitx5.addons = [ fcitx5-chinese-addons ]
```

#### X299:
```nix
i18n.inputMethod.fcitx5.addons = [ qt6Packages.fcitx5-chinese-addons ]
environment.variables.GTK_IM_MODULE = lib.mkForce ""
environment.variables.QT_IM_MODULE = lib.mkForce ""
```

### System Maintenance

#### M90aPro:
- Manual garbage collection

#### X299:
```nix
nix.gc = {
  automatic = true
  dates = "daily"
  options = "--delete-older-than 7d"
}
```

### Additional Services

#### X299 Only:
```nix
virtualisation.podman.enable = true
hardware.bluetooth.enable = true
hardware.bluetooth.powerOnBoot = true
services.upower.enable = true
services.gnome.gnome-keyring.enable = true
systemd.user.services.orca.enable = false
programs.dconf.enable = true
```

## Use Case Analysis

### M90aPro Profile:
- **Traditional desktop user**
- **Scientific/data analysis** work (Jupyter, pandas, scipy)
- **X11-based** workflow
- **High-resolution display** (2K)
- **Encrypted** system for security
- Prefers **Cinnamon** for familiarity

### X299 Profile:
- **Modern development workstation**
- **Wayland-first** approach with Hyprland
- **Containerization** with Podman
- **Extensive development tools** (LSP servers, direnv)
- **Advanced monitor control** (DDCCI)
- **Automated maintenance** (daily GC)
- Uses **cutting-edge** compositor and tools

## Migration Path

If you want to standardize features between hosts:

### To add Jupyter to X299:
Add to `X299-specific.nix`:
```nix
(python3.withPackages (python-pkgs: with python-pkgs; [
  jupyterlab numpy pandas matplotlib scipy
]))
```

### To add development tools to M90aPro:
Add to `M90aPro-specific.nix`:
```nix
devenv direnv nix-direnv nixd marksman
```

### To enable automatic GC on M90aPro:
Add to `M90aPro-specific.nix`:
```nix
nix.gc = {
  automatic = true
  dates = "weekly"  # or "daily"
  options = "--delete-older-than 14d"
}
```
