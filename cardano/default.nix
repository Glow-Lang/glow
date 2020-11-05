let
  pkgs = import ../pkgs.nix;
  gerbil-cardano-hs = pkgs.skipDocs (pkgs.gerbilCardanoHaskellPackages.callCabal2nix "gerbil-cardano-hs" ./haskell {});
  plutus = import (pkgs.thunkSource ../dep/plutus) {};
  plutus-scb = plutus.haskell.packages.plutus-scb.components.exes.plutus-scb;
  code-gen-script = pkgs.writeScript "codeGen" ''
    ${gerbil-cardano-hs}/bin/code-gen --output ./haskell-types
  '';
in
  pkgs.gerbil-support.gerbilPackage {
    pname = "gerbil-cardano";
    version = "0.1";
    gerbil-package = "mukn/cardano";
    gerbil = pkgs.gerbil-unstable;
    gambit-params = pkgs.gambit-support.unstable-params;
    gerbilInputs = with pkgs.gerbilPackages-unstable; [
      gerbil-utils
      gerbil-crypto
      gerbil-poo
      gerbil-persist
    ];
    buildInputs = [
      gerbil-cardano-hs
      code-gen-script
      plutus-scb
      gerbil-cardano-hs
    ];
    src = ./.;
    meta = {
      description = "Gerbil library for Cardano";
      homepage    = "https://gitlab.com/mukn/gerbil-cardano";
    };
  }