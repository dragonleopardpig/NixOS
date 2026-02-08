# Refactoring Changes Summary

## Changes Made

Based on your request, the configuration has been reorganized as follows:

### Moved to Common (configuration-common.nix)

The following items have been moved from X299-specific to common configuration, making them available on both hosts:

#### Development Tools:
- `devenv`, `direnv`, `nix-direnv`
- `claude-code`, `claude-monitor`
- `jsonrpc-glib`

#### Language Servers & Dev Tools:
- `nodejs`
- `yaml-language-server`
- `vscode-json-languageserver`
- `typescript-language-server`
- `bash-language-server`
- `systemd-language-server`
- `nginx-language-server`
- `kotlin-language-server`
- `perlnavigator`
- `nixd`
- `nix-index`
- `marksman`
- `gcc`
- Development libraries: `enchant`, `pkg-config`, `libxml2`, `glib`, `enchant2`

#### Spell Checking:
- `aspell` with dictionaries (en, en-science, en-computers)
- `hunspell` with en_US dictionary

#### Emacs:
- `emacs-pgtk` (Wayland-native version) with packages:
  - `vterm`
  - `direnv`
  - `lsp-pyright`
  - `zmq`

#### Hyprland Ecosystem:
- `gpustat`
- `hyprpaper`, `swww`
- `hyprsunset`, `hypridle`, `hyprsysteminfo`, `hyprshot`
- `waypaper`, `satty`, `slurp`, `grim`
- `flameshot`
- `xdg-desktop-portal`, `xdg-desktop-portal-hyprland`

#### System Maintenance:
- Automatic garbage collection:
  ```nix
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };
  ```

### Removed from M90aPro-specific

The following items have been completely removed from M90aPro configuration:

#### Bash Customization:
- ❌ Custom colorful bash prompt (entire `programs.bash` section)
- ❌ Shell aliases (ls, gc, rebuild)
- ❌ Fastfetch on shell init

#### Python/Scientific Packages:
- ❌ `python3.withPackages` including:
  - pandas, numpy, matplotlib
  - scipy, sympy
  - scikit-learn, scikit-image
  - jupyterlab, jupyter, ipykernel
  - pyzmq

#### Applications:
- ❌ `emacs-gtk` (replaced with emacs-pgtk in common)
- ❌ `nix-software-center`
- ❌ `nixos-conf-editor`
- ❌ `fzf` (already in common)

### What Remains Host-Specific

#### M90aPro-specific.nix (Now Much Simpler):
```nix
- Hostname: "M90aPro"
- NVIDIA config (imports nvidia.nix with PRIME for dual GPU)
- 2560x1440 resolution
- LightDM + Cinnamon desktop
- Fonts:
  - nerd-fonts.ubuntu*
  - noto-fonts*
- fcitx5-chinese-addons (non-qt6 version)
- system.stateVersion: "25.05"
```

**Note**: M90aPro has Intel iGPU + NVIDIA dGPU with PRIME sync enabled.

#### X299-specific.nix:
```nix
- Hostname: "X299"
- NVIDIA config (imports nvidia.nix for single GPU, no PRIME)
- 1920x1080 resolution
- Swap disabled
- DDCCI driver (monitor control)
- SDDM + Hyprland desktop
- Podman virtualization
- Bluetooth, UPower, GNOME Keyring
- Additional user groups: i2c, podman
- Themes (orchis, tela, fluent, sddm-astronaut)
- Wayland utilities: brightnessctl, wl-clipboard
- Applications: distrobox, wofi, rofi, walker, nemo-with-extensions
- Sioyek (XWayland wrapper)
- fcitx5-chinese-addons (qt6 version)
- Input method environment variables (GTK_IM_MODULE, QT_IM_MODULE)
- Additional fonts: nerd-fonts.caskaydia-cove
- system.stateVersion: "25.11"
```

**Note**: X299 has single NVIDIA GPU only (no integrated graphics).

## Impact Analysis

### M90aPro Gains:
✅ **Emacs-pgtk** - Modern Wayland-native Emacs (even though using Cinnamon/X11)
✅ **Development tools** - direnv, LSP servers, claude-code
✅ **Hyprland ecosystem** - Available if you want to switch to Hyprland
✅ **Automatic GC** - Daily cleanup to save disk space
✅ **Unified tooling** - Same dev environment as X299

### M90aPro Loses:
❌ **Python/Jupyter** - Scientific computing packages removed
❌ **Custom bash prompt** - Reverts to default
❌ **emacs-gtk** - Replaced with emacs-pgtk
❌ **nix-software-center/nixos-conf-editor** - GUI configuration tools

### X299 Changes:
- **Cleaner config** - Moved common items to shared config
- **Still has all functionality** - Nothing lost, just reorganized

## Migration Notes

### If You Need Python Back on M90aPro:

Add to `M90aPro-specific.nix`:
```nix
environment.systemPackages = with pkgs; [
  (python3.withPackages (python-pkgs: with python-pkgs; [
    jupyterlab numpy pandas matplotlib scipy sympy
    scikit-learn scikit-image
  ]))
];
```

### If You Need the Custom Bash Prompt Back:

Add to `M90aPro-specific.nix`:
```nix
programs.bash = {
  shellAliases = {
    ls = "eza --icons=always --group-directories-first --sort=extension";
    gc = "git commit -m";
    rebuild = "sudo nixos-rebuild switch";
  };
  interactiveShellInit = ''
    ${pkgs.fastfetch}/bin/fastfetch
  '';
  # ... (rest of prompt configuration)
};
```

### If You Need nix-software-center Back:

Add to `M90aPro-specific.nix`:
```nix
environment.systemPackages = with pkgs; [
  inputs.nix-software-center.packages.${system}.nix-software-center
  inputs.nixos-conf-editor.packages.${system}.nixos-conf-editor
];
```

## File Sizes

Before refactoring:
- M90aPro configuration.nix: **406 lines**
- X299 configuration.nix: **473 lines**
- Total unique lines: **~600+** (with duplication)

After refactoring:
- configuration-common.nix: **330 lines**
- M90aPro-specific.nix: **39 lines** ⬇️
- X299-specific.nix: **152 lines** ⬇️
- Total: **521 lines** (no duplication)

**Savings**: ~80+ lines removed through deduplication
**Maintainability**: Significantly improved - common changes only need to be made once

## Benefits

1. ✅ **Both hosts now have identical development environments**
2. ✅ **Both hosts have automatic garbage collection**
3. ✅ **Both hosts can run Hyprland if needed**
4. ✅ **Cleaner, more maintainable configuration**
5. ✅ **Easy to sync updates between hosts**
6. ✅ **M90aPro is now more focused (just display manager, resolution, fonts)**

## Next Steps

1. Copy the three new files to your NixOS configuration directory
2. Update your `configuration.nix` on each host to import the appropriate files
3. Test with `sudo nixos-rebuild test`
4. If successful, switch with `sudo nixos-rebuild switch`
5. (Optional) Add back any removed features you still need
