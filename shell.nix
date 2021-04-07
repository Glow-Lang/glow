{ ethereum ? true, thunk ? false, precompile ? false }:
let
  pkgs = import ./pkgs.nix;
  inherit (pkgs) lib glow-lang gerbil-support gerbilPackages-unstable nixpkgs thunkExe;
  inherit (gerbilPackages-unstable) gerbil-ethereum gerbil-poo;
  inherit (gerbil-support) gerbilLoadPath;
in
  pkgs.mkShell {
    inputsFrom = [
      glow-lang
    ];
    buildInputs = with pkgs; (
      glow-lang.buildInputs ++
      lib.optional ethereum go-ethereum ++
      # To speed this one up: nix path-info -f ./pkgs.nix -r thunkExe | cachix push mukn
      lib.optional thunk thunkExe ++
      [ netcat ] # used by integration tests
      );
    shellHook = ''
      echo ${gerbil-poo.src}; echo ${pkgs.gerbilPackages-unstable.gerbil-poo.src} ; echo
      echo ${glow-lang.src}; echo ${pkgs.gerbilPackages-unstable.glow-lang.src} ; echo ${pkgs.gerbilPackages-unstable.glow-lang.src} ; echo ${pkgs.gerbil-support.gerbilPackages-unstable.glow-lang.src} ; echo
      echo ${toString glow-lang.passthru.pre-pkg.gerbilInputs}
      echo ${gerbil-poo}

      ${glow-lang.postConfigure}
      export GERBIL_LOADPATH="${glow-lang.passthru.pre-pkg.testGerbilLoadPath}"
      PATH="${glow-lang.out}/bin:$PATH"
      GERBIL_APPLICATION_HOME="$PWD"
      GERBIL_APPLICATION_SOURCE="$PWD"
      GLOW_HOME="$PWD"
      GLOW_SRC="$PWD"
    '';
  }
