# Example uses:
#   nix-shell --pure
#   nix-shell --arg precompile true
{ ethereum ? true, precompile ? false }:
let
  pkgs = import ./pkgs.nix;
  inherit (pkgs) lib glow-lang gerbil-support gerbilPackages-unstable nixpkgs;
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
      [ netcat go-libp2p-daemon ] # used by integration tests
                                  # TODO: Save at compile time
                                  # the path to the p2pd (go-libp2p-daemon) binary.
                                  # The path is useful because it is a hash -
                                  # so nix knows we depend on this exact version.
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
