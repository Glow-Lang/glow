{ ethereum ? true }:
let
  pkgs = import ./pkgs.nix;
  lib = pkgs.lib;
  glow-lang = import ./default.nix;
in
  pkgs.mkShell {
    inputsFrom = [
      glow-lang
    ];
    buildInputs = lib.optional ethereum pkgs.go-ethereum;
    shellHook = ''
      ${glow-lang.postConfigure}
      ${lib.optionalString ethereum
        ''GERBIL_LOADPATH="$GERBIL_LOADPATH:${pkgs.gerbilPackages-unstable.gerbil-ethereum.src}"''}
    '';
    GERBIL_APPLICATION_HOME = "./";
  }
