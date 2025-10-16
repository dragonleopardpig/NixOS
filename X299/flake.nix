
# /etc/nixos/flake.nix
{
  description = "NixOS configuration";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    # Add grub2 themes to your inputs ...
    grub2-themes.url = "github:vinceliuice/grub2-themes";
    nix-software-center.url = "github:snowfallorg/nix-software-center";
    nixos-conf-editor.url = "github:snowfallorg/nixos-conf-editor";
  };
  
  outputs = { self, nixpkgs, grub2-themes, ... }@inputs: {
    nixosConfigurations.X299 = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      # ... and then to your modules
      modules = [
        ./configuration.nix
        grub2-themes.nixosModules.default
      ];
    };
  };
}
