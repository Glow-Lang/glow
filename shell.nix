{ ethereum ? false }:
let
  pkgs = import ./pkgs.nix;
  glow = import ./default.nix;
  ethereumPkgs = if ethereum then [ pkgs.go-ethereum pkgs.solc ] else [];
in
  pkgs.mkShell {
    inputsFrom = [
      glow
    ];
    buildInputs = [
      pkgs.thunkExe
    ] ++  ethereumPkgs;
    shellHook = glow.postConfigure;
    GERBIL_APPLICATION_HOME = "./";
  }