{ config, pkgs, ... }:
{
 # Enable OpenGL
  hardware.graphics = {
    enable = true;
    # driSupport = true;
    # driSupport32Bit = true;
  };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {

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

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    # package = config.boot.kernelPackages.nvidiaPackages.beta;

    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "580.95.05";
      sha256_64bit = "sha256-hJ7w746EK5gGss3p8RwTA9VPGpp2lGfk5dlhsv4Rgqc=";
      sha256_aarch64 = "sha256-zLRCbpiik2fGDa+d80wqV3ZV1U1b4lRjzNQJsLLlICk=";
      openSha256 = "sha256-RFwDGQOi9jVngVONCOB5m/IYKZIeGEle7h0+0yGnBEI=";
      settingsSha256 = "sha256-F2wmUEaRrpR1Vz0TQSwVK4Fv13f3J9NJLtBe4UP2f14=";
      persistencedSha256 = "sha256-QCwxXQfG/Pa7jSTBB0xD3lsIofcerAWWAHKvWjWGQtg=";
    };
    
  };

  # hardware.nvidia = {
  #   modesetting.enable = true;
  #   nvidiaSettings = true;
  #   open = false;  # Use proprietary driver, not nouveau
  #   package = config.boot.kernelPackages.nvidiaPackages.beta;
  # };

  # Enable X server with NVIDIA driver
  # services.xserver = {
  #   enable = true;
  #   videoDrivers = [ "nvidia" ];
  # };

#   # Environment variables for Wayland or X11 sessions
#   environment.sessionVariables = {
#     GBM_BACKEND = "nvidia-drm";
#     LIBVA_DRIVER_NAME = "nvidia";
#     __GLX_VENDOR_LIBRARY_NAME = "nvidia";
#     XDG_SESSION_TYPE = "x11"; # Change to "x11" if you use X11
#   };

#   # Accept NVIDIA license
#   nixpkgs.config.nvidia.acceptLicense = true;

}
