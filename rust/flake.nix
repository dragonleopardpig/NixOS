{
  description = "Dioxus hot_dog project with devenv";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";

    # Add rust-overlay
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, devenv, rust-overlay, ... }:
    devenv.lib.mkFlake {
      inherit inputs;
      modules = [ ./devenv.nix ];
    };
}
