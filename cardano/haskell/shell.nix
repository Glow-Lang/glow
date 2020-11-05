let
  pkgs = import ../../pkgs.nix;
  glow-cardano = pkgs.skipDocs (pkgs.gerbilCardanoHaskellPackages.callCabal2nix "glow-cardano" ./. {});
  plutus = import (pkgs.thunkSource ../../dep/plutus) {};
  plutus-scb = plutus.haskell.packages.plutus-scb.components.exes.plutus-scb;
in
  pkgs.mkShell {
    inputsFrom = [
      glow-cardano.env
    ];
    buildInputs = [
      glow-cardano
      plutus-scb
      pkgs.thunkExe
      pkgs.haskellPackages.hoogle
    ];
  }