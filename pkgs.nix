let
  pkgs = import <nixpkgs> { inherit config; };

  inherit (pkgs.lib.POP) kxPop extendPop;
  inherit (pkgs.gerbil-support) path-src overrideSrcIfShaDiff gerbilFilterSource;

  config = {
    packageOverrides = pkgs: rec {

      gerbil-support =
        extendPop pkgs.gerbil-support (gerbil-support: super: {
          # Skip extra files so that we can play with CI configuration yet have the CI reuse cached builds.
          gerbilSkippableFiles = super.gerbilSkippableFiles ++ [".gitlab.yml" ".github" "pkgs.nix" "shell.nix" "default.nix" "docs" "future" "dep" "Dockerfile" "Dockerfile.nixos" "glow-install" "ci.ss" ".build"];

          prePackages-unstable = extendPop super.prePackages-unstable (_: super:
            { "glow-lang" = extendPop super.glow-lang (params: super:
              let source = gerbilFilterSource ./.; in
              { pre-src = path-src source;
                inherit source;
                inherit pkgs;
                testGerbilLoadPath = "${
                  gerbil-support.gerbilLoadPath ([pkgs.glow-lang] ++ params.gerbilInputs)}:${
                  source}";});});});
    };
  };
in
  pkgs
