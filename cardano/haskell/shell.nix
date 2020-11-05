let
  pkgs = import ../../pkgs.nix;
  nix-thunk-exe = pkgs.ignoreVersionBounds (pkgs.nixThunkHaskellPackages.callCabal2nix "nix-thunk" (pkgs.thunkSource ../dep/nix-thunk) {});
  gerbil-cardano-hs = pkgs.skipDocs (pkgs.gerbilCardanoHaskellPackages.callCabal2nix "gerbil-cardano-hs" ./. {});
  plutus = import (pkgs.thunkSource ../dep/plutus) {};
  plutus-scb = plutus.haskell.packages.plutus-scb.components.exes.plutus-scb;
in
  pkgs.mkShell {
    inputsFrom = [
      gerbil-cardano-hs.env
    ];
    buildInputs = [
      # gerbil-cardano-hs
      nix-thunk-exe
      plutus-scb
      pkgs.haskellPackages.hoogle
    ];
  }