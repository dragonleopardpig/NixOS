# flake.nix
{
  description = "NixOS configuration";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    # Add grub2 themes to your inputs ...
    grub2-themes = {
      url = "github:vinceliuice/grub2-themes";
    };
  };
  outputs = inputs@{ nixpkgs,  grub2-themes, ... }: {
    nixosConfigurations = {
      X299 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        # ... and then to your modules
        modules = [
          ./configuration.nix
          grub2-themes.nixosModules.default
        ];
      };
    };
  };
}
