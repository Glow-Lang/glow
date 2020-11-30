{ ethereum ? true
}:
let
  pkgs = import ./pkgs.nix;
  lib = pkgs.lib;
  glow-lang = import ./default.nix;
  gerbil-ethereum-local = pkgs.gerbilPackages-unstable.gerbil-ethereum.overrideAttrs (old: {
    src = pkgs.thunkSource ./dep/gerbil-ethereum;
  });
in
  pkgs.mkShell {
    inputsFrom = [
      glow-lang
    ];
    buildInputs = lib.optional ethereum pkgs.go-ethereum ++ [
      pkgs.thunkExe
    ];
    shellHook = ''
      ${glow-lang.postConfigure}
      ${lib.optionalString ethereum
        ''GERBIL_LOADPATH="$GERBIL_LOADPATH:${gerbil-ethereum-local.src}"''}
    '';
    GERBIL_APPLICATION_HOME = "./";
  }
