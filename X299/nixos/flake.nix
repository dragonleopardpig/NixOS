# /etc/nixos/flake.nix
{
  description = "NixOS configuration";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    grub2-themes.url = "github:vinceliuice/grub2-themes";
    catppuccin.url = "github:catppuccin/nix/release-25.05";
    # nix-software-center.url = "github:snowfallorg/nix-software-center";
    # nixos-conf-editor.url = "github:snowfallorg/nixos-conf-editor";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  
  outputs = inputs@{ nixpkgs, grub2-themes, catppuccin, home-manager, ... }:
    {
      nixosConfigurations.X299 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        # ... and then to your modules
        modules = [
          ./configuration.nix
          grub2-themes.nixosModules.default
          catppuccin.nixosModules.catppuccin
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            
            # TODO replace ryan with your own username
            home-manager.users.thinky = {
              imports = [
                ./home.nix
                catppuccin.homeModules.catppuccin
              ];
            };
            # Optionally, use home-manager.extraSpecialArgs
            # to pass arguments to home.nix
          }
        ];
      };
    };
}
