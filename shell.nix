let
  pkgs = import ./pkgs.nix;
  glow = import ./default.nix;
in
  pkgs.mkShell {
    inputsFrom = [
      glow
    ];
  }