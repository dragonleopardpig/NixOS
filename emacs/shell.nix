# nix-shell and start emacs from shell, open try.org file
# this will help Jinx to compile
# refer https://github.com/minad/jinx/discussions/104

with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [
    enchant
  ];
}
