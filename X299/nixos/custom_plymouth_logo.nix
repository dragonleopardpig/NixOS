{ lib, stdenv }:

stdenv.mkDerivation {
  name = "plymouth-icon";
  version = "1.0";

  src = ./assets/nix-snowflake-rainbow.png;

  unpackPhase = "true"; # Skip the unpack phase

  installPhase = ''
    mkdir -p $out/share/icons/hicolor/128x128/apps
    cp $src $out/share/icons/hicolor/128x128/apps/nix-snowflake-rainbow.png
  '';

  meta = {
    description = "Icon for Plymouth";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ bashfulrobot ];
  };
}
