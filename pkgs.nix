let
  pkgs = import <nixpkgs> { inherit config; };

  inherit (pkgs.lib.POP) kxPop extendPop;
  inherit (pkgs.gerbil-support) path-src overrideSrcIfShaDiff gerbilFilterSource;

  config = {
    packageOverrides = pkgs: rec {

      gerbil-support =
        extendPop pkgs.gerbil-support (gerbil-support: superGS: {
          # Skip extra files so that we can play with CI configuration yet have the CI reuse cached builds.
          gerbilSkippableFiles = superGS.gerbilSkippableFiles ++ [".gitlab.yml" ".github" "pkgs.nix" "shell.nix" "default.nix" "docs" "future" "dep" "Dockerfile" "Dockerfile.nixos" "glow-install" "ci.ss" ".build"];

          prePackages-unstable = extendPop superGS.prePackages-unstable (_: superPPU:
            { "glow-lang" = extendPop superPPU.glow-lang (params: superGL:
              let source = gerbilFilterSource ./.; in
              { pre-src = path-src source;
                inherit source;
                inherit pkgs;
                });});});

      testGerbilLoadPath = let pre = pkgs.glow-lang.passthru.pre-pkg; in
        "${gerbil-support.gerbilLoadPath ([pkgs.glow-lang] ++ pre.gerbilInputs)}:${pre.source}";

    };


  };
in
  pkgs
