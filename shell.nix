{ ethereum ? true, thunk ? true, precompile ? false }:
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
      lib.optional thunk thunkExe);
    shellHook = ''
      echo ${gerbil-poo.src}; echo ${pkgs.gerbilPackages-unstable.gerbil-poo.src} ; echo
      echo ${glow-lang.src}; echo ${pkgs.gerbilPackages-unstable.glow-lang.src} ; echo ${pkgs.gerbilPackages-unstable.glow-lang.src} ; echo ${pkgs.gerbil-support.gerbilPackages-unstable.glow-lang.src} ; echo
      echo ${toString glow-lang.passthru.pre-pkg.gerbilInputs}
      echo ${gerbil-poo}

      ${glow-lang.postConfigure}
      ${lib.optionalString ethereum
        ''export GERBIL_ETHEREUM_SRC=${gerbil-ethereum.src}
        export GERBIL_LOADPATH="$PWD:${gerbilLoadPath ([(if precompile then glow-lang else "$out")] ++ glow-lang.passthru.pre-pkg.gerbilInputs)}:$GERBIL_ETHEREUM_SRC"''}
    '';
    GERBIL_APPLICATION_HOME = "${glow-lang.src}";
    GERBIL_APPLICATION_SOURCE = "${glow-lang.src}";
    GLOW_HOME = "${glow-lang.src}";
    GLOW_SRC = "${glow-lang.src}";
  }
