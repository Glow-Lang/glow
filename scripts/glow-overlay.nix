# You can symlink this overlay into your ~/.config/nixpkgs/overlays/
# to make glow-lang available to your nix-env -iA glow-lang.
# TODO: try src = builtins.fetchGit ./. and extracting version from THAT, and/or lib.commitIdFromGitRepo.
# TODO: For every dep, 1. override the params, not the packages, and 2. also override the version and git-version.
pkgs: super: rec {
  inherit (super) lib fetchFromGitLab fetchgit
     gerbil-unstable gambit-support gerbil-support gerbilPackages-unstable;

  ver = if builtins.pathExists ./../version.ss
        then let m =
          builtins.match "\\(import :clan/versioning.*\\)\n\\(register-software \"([-_.A-Za-z0-9]+)\" \"([-_.A-Za-z0-9]+)\"\\) ;; ([-0-9]+)\n"
            (builtins.readFile ./../version.ss); in
            { version = builtins.elemAt m 2; git-version = builtins.elemAt m 1; }
        else { version = "0.0"; git-version = "0.0"; };

  # TODO: override the params in such a way that the fixed-point is taken correctly.
  gerbil-poo-override = gerbilPackages-unstable.gerbil-poo.overrideAttrs (old: {
    src = pkgs.thunkSource ../dep/gerbil-poo;
  });

  gerbil-ethereum-override = gerbilPackages-unstable.gerbil-ethereum.overrideAttrs (old: {
    src = pkgs.thunkSource ../dep/gerbil-ethereum;
  });

  gerbil-utils-override = gerbilPackages-unstable.gerbil-utils.overrideAttrs (old: {
    src = pkgs.thunkSource ../dep/gerbil-utils;
  });

  glow-lang = gerbil-support.gerbilPackage
    ( gerbil-support.gerbilPackages-unstable-params.glow-lang // {
        version = ver.version;
        git-version = ver.git-version;
        gerbilInputs = with gerbilPackages-unstable;
          [gerbil-utils-override gerbil-crypto gerbil-poo-override gerbil-persist
           gerbil-ethereum-override smug-gerbil gerbil-libp2p];
        src = builtins.filterSource
          (path: type: let baseName = baseNameOf path; in
            ! (builtins.elem baseName [".git" "run" "result" "dep" "BLAH"] ||
               lib.hasSuffix "~" baseName))
          ./..;
      });
}
