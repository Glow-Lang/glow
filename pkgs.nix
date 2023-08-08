let
  pkgs = import <nixpkgs> { inherit config; }; # selfPkgs
  inherit (pkgs) lib;
  _bar = lib.debug.traceVal "FOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO";

  config = {
    packageOverrides = superPkgs:
      let superGS = superPkgs.gerbil-support;
          superPPU = superGS.prePackages-unstable;
          inherit (superGS) path-src overrideSrcIfShaDiff gerbilFilterSource gerbilLoadPath
                  gerbilVersionFromGit;
          source = gerbilFilterSource ./.; in
      rec {
        gerbil-support = superGS // {
          inherit pkgs;
          # Skip extra files so we can play with CI configuration yet have the CI reuse cached builds.
          gerbilSkippableFiles = superGS.gerbilSkippableFiles ++
            [".gitlab.yml" ".github" "pkgs.nix" "shell.nix" "default.nix" "docs" "future" "dep"
             "Dockerfile" "Dockerfile.nixos" "glow-install" "ci.ss" ".build"];

          prePackages-unstable =
            let glow-lang = superPPU.glow-lang //
                    { pre-src = path-src source; inherit pkgs source;} //
                    gerbilVersionFromGit source "version"; in
            superPPU // { inherit glow-lang;};};

        glow-lang = gerbil-support.gerbilPackages-unstable.glow-lang;

        testGerbilLoadPath =
          "${gerbilLoadPath ([glow-lang] ++ glow-lang.passthru.pre-pkg.gerbilInputs)}:${source}";

      };}; in
  pkgs
