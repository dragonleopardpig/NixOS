# run: nix develop --no-pure-eval

{
  description = "Rust + Dioxus devShell using devenv";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devenv.url = "github:cachix/devenv";

    # 🔴 REQUIRED for languages.rust.channel
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, devenv, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };
      in {
        devShells.default =
          devenv.lib.mkShell {
            inherit pkgs inputs;

            modules = [
              {
                languages.rust = {
                  enable = true;
                  channel = "stable";
                  targets = [ "wasm32-unknown-unknown" ];
                };

                env.packages = with pkgs; [
                  rust-analyzer
                  cargo-binstall
                ];

                # shell hook installs exact wasm-bindgen-cli version
                enterShell = ''
      # install version 0.2.106 if not installed
      if ! command -v wasm-bindgen >/dev/null || [[ $(wasm-bindgen --version) != "0.2.106" ]]; then
        echo "Installing wasm-bindgen-cli v0.2.106 via cargo-binstall..."
        cargo binstall --force --version 0.2.106 wasm-bindgen-cli
      fi
    '';
              }
            ];

          };
      }
    );
}
