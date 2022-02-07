let
  pkgs = import ../haskellPkgs.nix;
  glow-cardano = pkgs.skipDocs (pkgs.gerbilCardanoHaskellPackages.callCabal2nix "glow-cardano" ./haskell {});
  plutus = import (pkgs.thunkSource ../dep/plutus) {};
  plutus-scb = plutus.haskell.packages.plutus-scb.components.exes.plutus-scb;
  code-gen-script = pkgs.writeScript "codeGen" ''
    ${glow-cardano}/bin/code-gen --output ./haskell-types
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
      glow-cardano
      code-gen-script
      plutus-scb
    ];
    src = ./.;
    meta = {
      description = "Glow support for Cardano";
      homepage    = "https://github.com/Glow-lang/glow";
    };
  }