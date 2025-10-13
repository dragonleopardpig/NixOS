{ config, pkgs, ... }:
{
 # Enable OpenGL
  hardware.graphics = {
    enable = true;
  };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;

    # package = config.boot.kernelPackages.nvidiaPackages.production.overrideAttrs {
    #     version = "580.76.05";
    #     src = pkgs.fetchurl {
    #       url = "https://us.download.nvidia.com/XFree86/Linux-x86_64/580.76.05/NVIDIA-Linux-x86_64-580.76.05.run";
    #       sha256 = "1zcpbp859h5whym0r54a3xrkqdl7z3py1hg8n8hv0c89nqvfd6r1";
    #     };
    # };
    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    powerManagement.enable = false;
    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of 
    # supported GPUs is at: 
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus 
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
	# accessible via `nvidia-settings`.
    nvidiaSettings = true;
    
  };

  #  hardware.nvidia.prime = {
  #   # ... other prime options ...
  #   # 00:02.0 VGA compatible controller: Intel Corporation Arrow Lake-S [Intel Graphics] (rev 06)
  #   # 02:00.0 3D controller: NVIDIA Corporation AD107M [GeForce RTX 4050 Max-Q / Mobile] (rev a1)
  #   sync.enable = true;
  #   intelBusId = "PCI:0:2:0"; # Example for Intel GPU at 00:02.0
  #   nvidiaBusId = "PCI:2:0:0"; # Example for NVIDIA GPU at 02:00.0
  # };
}
