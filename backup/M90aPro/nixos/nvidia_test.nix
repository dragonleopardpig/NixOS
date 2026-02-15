{ config, lib, pkgs, ... }:

let
  # This script is a convenient wrapper to run programs with Nvidia offload
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec "$@"
  '';
in
{
  # Allow unfree packages for Nvidia drivers
  nixpkgs.config.allowUnfree = true;
   # Enable OpenGL
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware = {
    nvidia = {
      # Enable modesetting and power management
      modesetting.enable = true;
      powerManagement = {
        enable = true;
        # finegrained = true;
      };
      # Configure Prime offload
      prime = {
        sync.enable = true;
        # offload.enable = true;
        # offload.enableOffloadCmd = true;
        # Replace these with your actual PCI bus IDs found via lspci
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:2:0:0";
      };
      open = false;
      # Enable the Nvidia settings menu,
	    # accessible via `nvidia-settings`.
      nvidiaSettings = true;

      # Optionally, you may need to select the appropriate driver version for your specific GPU.
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
  };

  # Add the offload script to your system packages
  environment.systemPackages = [ nvidia-offload ];

  # Other X server settings (adjust as needed)
  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" "modesetting" ];
    # ... other settings (display manager, desktop manager, etc.)
  };
}
