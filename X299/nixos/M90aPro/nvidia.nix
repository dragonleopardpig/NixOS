# /etc/nixos/M90aPro/nvidia.nix
# NVIDIA PRIME configuration for M90aPro (Intel + NVIDIA)
{ config, pkgs, ... }:
{
  hardware.graphics = {
    enable = true;
  };

  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;

    prime = {
      sync.enable = true;
      # 00:02.0 VGA compatible controller: Intel Corporation Arrow Lake-S [Intel Graphics]
      intelBusId = "PCI:0:2:0";
      # 02:00.0 3D controller: NVIDIA Corporation AD107M [GeForce RTX 4050 Max-Q / Mobile]
      nvidiaBusId = "PCI:2:0:0";
    };
  };
}
