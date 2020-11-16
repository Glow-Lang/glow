{ ethereum ? false }:
let
  pkgs = import ./pkgs.nix;
  glow = import ./default.nix;
  ethereumPkgs = if ethereum then [ pkgs.go-ethereum ] else [];
in
  pkgs.mkShell {
    inputsFrom = [
      glow
    ];
    buildInputs = ethereumPkgs;
    shellHook = glow.postConfigure;
    GERBIL_APPLICATION_HOME = "./";
  }