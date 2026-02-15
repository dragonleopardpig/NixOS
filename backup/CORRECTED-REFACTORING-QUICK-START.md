# Corrected Refactoring - Quick Start Guide

## What Happened

I initially compared **old/outdated files** from `~/Downloads/NixOS/M90aPro/` instead of your **actual running config** at `/etc/nixos/configuration.nix`.

After comparing the correct files, I discovered both systems are already ~95% identical!

## New Files Created (CORRECTED)

### Common Configuration:
- `configuration-common-CORRECTED.nix` - All shared settings (~400 lines)

### Host-Specific:
- `M90aPro-specific-CORRECTED.nix` - M90aPro only (~60 lines)
- `X299-specific-CORRECTED.nix` - X299 only (~70 lines)

### Example Configs:
- `configuration-M90aPro-CORRECTED-example.nix` - Copy as configuration.nix
- `configuration-X299-CORRECTED-example.nix` - Copy as configuration.nix

### Documentation:
- `ACTUAL-COMPARISON-SUMMARY.md` - Detailed comparison
- `CORRECTED-REFACTORING-QUICK-START.md` - This file

## Actual Differences Between Hosts

### M90aPro:
- Dual GPU (Intel + NVIDIA) with nvidia_offload.nix
- 2560x1440 resolution
- **Cinnamon + Hyprland** (both available!)
- SDDM theme: pixel_sakura
- emacs-gtk
- Console font: terminus
- Blueman enabled
- Extra packages: mesa-demos, efibootmgr, gptfdisk, util-linux, lua

### X299:
- Single NVIDIA GPU with nvidia.nix
- 1920x1080 resolution
- **Hyprland only**
- SDDM theme: cyberpunk
- emacs-pgtk (Wayland-native)
- DDCCI driver (monitor brightness control)
- Swap disabled
- User groups: +i2c, +podman
- Extra services: UPower, GNOME Keyring
- Extra packages: nemo-with-extensions, claude-monitor

## Quick Deploy on M90aPro

```bash
cd ~/Downloads/NixOS
sudo cp /etc/nixos/configuration.nix /etc/nixos/configuration.nix.backup
sudo cp configuration-common-CORRECTED.nix /etc/nixos/
sudo cp M90aPro-specific-CORRECTED.nix /etc/nixos/
sudo cp configuration-M90aPro-CORRECTED-example.nix /etc/nixos/configuration.nix
sudo nixos-rebuild test
# If OK:
sudo nixos-rebuild switch
```

## Quick Deploy on X299

```bash
cd ~/Downloads/NixOS
# Copy files
sudo cp configuration-common-CORRECTED.nix /etc/nixos/
sudo cp X299-specific-CORRECTED.nix /etc/nixos/
sudo cp configuration-X299-CORRECTED-example.nix /etc/nixos/configuration.nix
sudo nixos-rebuild test
# If OK:
sudo nixos-rebuild switch
```

## What's Shared (Moved to Common)

✅ **Everything except the differences listed above!** Including:
- Hyprland + SDDM + UWSM
- All development tools (claude-code, direnv, LSP servers)
- Hyprland ecosystem (hyprpaper, hyprshot, waypaper, etc.)
- Podman
- Automatic daily GC
- fcitx5 with qt6Packages.fcitx5-chinese-addons
- All common fonts
- 95%+ of packages

## Files to Ignore

These were based on old/incorrect comparisons:
- ❌ `configuration-common.nix` (original)
- ❌ `M90aPro-specific.nix` (original)
- ❌ `X299-specific.nix` (original)
- ❌ `REFACTORING-GUIDE.md` (outdated)
- ❌ `REFACTORING-CHANGES.md` (outdated)
- ❌ `COMPARISON.md` (outdated)

## Files to Use

Use these **CORRECTED** versions:
- ✅ `configuration-common-CORRECTED.nix`
- ✅ `M90aPro-specific-CORRECTED.nix`
- ✅ `X299-specific-CORRECTED.nix`
- ✅ `ACTUAL-COMPARISON-SUMMARY.md`
- ✅ `NVIDIA-CONFIG-NOTES.md` (still valid)

## Verification

After deploying, verify with:

```bash
# Check hostname
hostname

# Check NVIDIA
nvidia-smi

# Check desktop environments available
ls /run/current-system/sw/share/wayland-sessions/
ls /run/current-system/sw/share/xsessions/

# Check if Hyprland works
echo $XDG_SESSION_DESKTOP

# Check emacs version
emacs --version | head -1
```

## Need Help?

See `ACTUAL-COMPARISON-SUMMARY.md` for:
- Detailed comparison table
- Migration steps
- What changed from incorrect attempt
- Complete file structure

## Summary

The corrected refactoring shows both systems are already very similar. The main differences are:
1. **Hardware** - Dual vs single GPU, monitor resolution, DDCCI
2. **Desktop** - M90aPro has Cinnamon option, X299 Hyprland-only
3. **Emacs** - GTK vs pgtk (Wayland)
4. **Minor config** - Themes, fonts, a few extra packages

Everything else (95%+ of config) is now in the common file!
