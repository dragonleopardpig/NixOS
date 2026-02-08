# NixOS Configuration Refactoring Guide

## Overview
Your NixOS configurations have been refactored into three main files:
- **configuration-common.nix** - Shared settings between both hosts
- **M90aPro-specific.nix** - Settings unique to M90aPro
- **X299-specific.nix** - Settings unique to X299

## File Structure

```
/etc/nixos/  (or your config directory)
├── configuration.nix           # Main config (imports common + host-specific)
├── configuration-common.nix    # Shared configuration
├── M90aPro-specific.nix       # M90aPro-specific settings
├── X299-specific.nix          # X299-specific settings
├── hardware-configuration.nix  # Auto-generated hardware config
├── nvidia.nix                  # NVIDIA settings (HOST-SPECIFIC!)
└── custom_plymouth_logo.nix    # Plymouth logo package
```

**Important**: `nvidia.nix` is different for each host:
- **M90aPro**: Dual GPU (Intel + NVIDIA) with PRIME sync configuration
- **X299**: Single NVIDIA GPU, no PRIME needed

## Key Differences Between Hosts

### M90aPro-Specific Features:
- **Desktop**: LightDM + Cinnamon (X11)
- **Display**: 2560x1440 resolution
- **LUKS encryption** enabled
- **Packages**:
  - emacs-gtk
  - Python with Jupyter/scientific packages
  - nix-software-center, nixos-conf-editor
- **Shell**: Custom colorful bash prompt
- **Input**: fcitx5-chinese-addons

### X299-Specific Features:
- **Desktop**: SDDM + Hyprland (Wayland)
- **Display**: 1920x1080 resolution
- **No swap** (disabled)
- **DDCCI driver** for monitor brightness control
- **Virtualization**: Podman enabled
- **Packages**:
  - emacs-pgtk (Wayland-native Emacs)
  - Hyprland ecosystem (hyprpaper, waybar, etc.)
  - Development tools (direnv, claude-code, LSP servers)
  - Distrobox, wofi, rofi, walker
- **Input**: qt6Packages.fcitx5-chinese-addons
- **Nix GC**: Automatic daily garbage collection
- **Extra fonts**: nerd-fonts.caskaydia-cove
- **User groups**: i2c, podman
- **Trusted users**: root, thinky

## How to Use

### For M90aPro:
Replace your `configuration.nix` with:
```nix
{ inputs, lib, config, pkgs, ... }:

{
  imports = [
    ./configuration-common.nix
    ./M90aPro-specific.nix
  ];
}
```

### For X299:
Replace your `configuration.nix` with:
```nix
{ inputs, lib, config, pkgs, ... }:

{
  imports = [
    ./configuration-common.nix
    ./X299-specific.nix
  ];
}
```

## Migration Steps

1. **Backup your current configuration**:
   ```bash
   sudo cp /etc/nixos/configuration.nix /etc/nixos/configuration.nix.backup
   ```

2. **Copy the new files** to `/etc/nixos/`:
   ```bash
   sudo cp configuration-common.nix /etc/nixos/
   sudo cp M90aPro-specific.nix /etc/nixos/  # or X299-specific.nix
   ```

3. **Copy the correct nvidia.nix for your host**:
   - For M90aPro:
     ```bash
     sudo cp nvidia-M90aPro.nix /etc/nixos/nvidia.nix
     ```
   - For X299:
     ```bash
     sudo cp nvidia-X299.nix /etc/nixos/nvidia.nix
     ```
   - **Or keep your existing nvidia.nix** if it's already configured correctly

4. **Update your main configuration.nix**:
   - For M90aPro: Use the example from `configuration-M90aPro-example.nix`
   - For X299: Use the example from `configuration-X299-example.nix`

5. **Test the configuration**:
   ```bash
   sudo nixos-rebuild test
   ```

6. **If successful, switch permanently**:
   ```bash
   sudo nixos-rebuild switch
   ```

## Benefits of This Structure

1. **Maintainability**: Common settings are in one place
2. **Clarity**: Host-specific settings are clearly separated
3. **Consistency**: Ensures both hosts share the same base configuration
4. **Easier updates**: Update common settings once for both hosts
5. **DRY principle**: Don't Repeat Yourself - shared code is written once

## Notes

- Both hosts still need their own `hardware-configuration.nix` (auto-generated)
- Both hosts share the same `nvidia.nix` and `custom_plymouth_logo.nix`
- The `inputs` parameter is used for flake inputs (nix-software-center, nixos-conf-editor, hyprland)
- Make sure to keep the `system.stateVersion` in the host-specific files (don't change after initial install)

## Future Improvements

You could further modularize by creating:
- `desktop-environments/cinnamon.nix`
- `desktop-environments/hyprland.nix`
- `packages/common.nix`
- `packages/development.nix`
- `packages/scientific.nix`

This would make the configuration even more flexible and reusable.
