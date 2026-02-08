# NVIDIA Configuration Notes

## GPU Setup Differences

### M90aPro (Laptop)
- **Hardware**: Dual GPU setup
  - Intel Arrow Lake-S integrated graphics (PCI:0:2:0)
  - NVIDIA GeForce RTX 4050 Max-Q Mobile (PCI:2:0:0)
- **Configuration**: NVIDIA PRIME with sync mode
- **File**: `nvidia-M90aPro.nix` → copy as `nvidia.nix`

### X299 (Desktop)
- **Hardware**: Single GPU setup
  - NVIDIA discrete GPU only
- **Configuration**: Standard NVIDIA driver, no PRIME
- **File**: `nvidia-X299.nix` → copy as `nvidia.nix`

## NVIDIA PRIME Modes (M90aPro Only)

### Currently Using: Sync Mode
```nix
hardware.nvidia.prime = {
  sync.enable = true;
  intelBusId = "PCI:0:2:0";
  nvidiaBusId = "PCI:2:0:0";
};
```

**Pros:**
- Better performance (both GPUs active)
- No need to manually specify which apps use NVIDIA
- Smoother experience for graphics-intensive tasks

**Cons:**
- Higher power consumption
- Shorter battery life
- Both GPUs always active

### Alternative: Offload Mode
```nix
hardware.nvidia.prime = {
  offload.enable = true;
  offload.enableOffloadCmd = true;
  intelBusId = "PCI:0:2:0";
  nvidiaBusId = "PCI:2:0:0";
};
```

**Pros:**
- Better battery life
- Lower power consumption
- NVIDIA GPU only used when needed

**Cons:**
- Need to manually specify which apps use NVIDIA:
  ```bash
  nvidia-offload <command>
  ```
- Slightly more complex to use

## How to Switch Modes (M90aPro)

1. Edit `/etc/nixos/nvidia.nix`
2. For sync mode:
   - `sync.enable = true;`
   - Comment out offload lines
3. For offload mode:
   - Comment out `sync.enable = true;`
   - Uncomment offload lines
4. Rebuild: `sudo nixos-rebuild switch`

## Verifying Your GPU Setup

### Check PCI Bus IDs
```bash
lspci | grep -E "VGA|3D"
```

Expected output on M90aPro:
```
00:02.0 VGA compatible controller: Intel Corporation Arrow Lake-S [Intel Graphics]
02:00.0 3D controller: NVIDIA Corporation AD107M [GeForce RTX 4050 Max-Q / Mobile]
```

Expected output on X299:
```
XX:XX.X VGA compatible controller: NVIDIA Corporation ...
```

### Check Active NVIDIA Driver
```bash
nvidia-smi
```

### Check PRIME Status (M90aPro only)
```bash
glxinfo | grep "OpenGL renderer"
```

In sync mode, you should see NVIDIA GPU.
In offload mode, you'll see Intel GPU unless using `nvidia-offload`.

## Troubleshooting

### Screen Tearing
Add to nvidia.nix:
```nix
services.xserver.screenSection = ''
  Option "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
'';
```

### Suspend/Resume Issues
Try enabling power management:
```nix
hardware.nvidia.powerManagement.enable = true;
```

### Check if NVIDIA is Being Used
```bash
# See which GPU is rendering
glxinfo | grep vendor

# Monitor GPU usage
nvidia-smi -l 1
```

## Files in This Directory

- `nvidia-M90aPro.nix` - NVIDIA config for M90aPro (PRIME sync)
- `nvidia-X299.nix` - NVIDIA config for X299 (single GPU)
- Copy the appropriate file as `nvidia.nix` in your `/etc/nixos/` directory
