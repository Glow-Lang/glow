# You can symlink this overlay into your ~/.config/nixpkgs/overlays/
# to make glow-lang available to your nix-env -iA glow-lang.
# TODO: try src = builtins.fetchGit ./. and extracting version from THAT, and/or lib.commitIdFromGitRepo.
pkgs: super: rec {
  inherit (super) lib fetchFromGitLab fetchgit
     gerbil-unstable gambit-support gerbil-support gerbilPackages-unstable;

  ver = if builtins.pathExists ./version.ss
        then let m =
          builtins.match "\\(import :utils/versioning\\)\n\\(register-software \"([-_.A-Za-z0-9]+)\" \"([-_.A-Za-z0-9]+)\"\\) ;; ([-0-9]+)\n"
            (builtins.readFile ./version.ss); in
            { version = builtins.elemAt m 2; git-version = builtins.elemAt m 1; }
        else { version = "0.0"; git-version = "0.0"; };

  glow-lang = gerbil-support.gerbilPackage {
    pname = "glow-lang";
    version = ver.version;
    git-version = ver.git-version;
    gerbil-package = "mukn/glow";
    gerbil = gerbil-unstable;
    buildInputs = [];
    gambit-params = gambit-support.unstable-params;
    softwareName = "Glow";
    gerbilInputs = with gerbilPackages-unstable;
      [gerbil-utils gerbil-crypto gerbil-poo gerbil-persist gerbil-libp2p gerbil-ethereum smug-gerbil];
    version-path = "version";
    src = builtins.filterSource
      (path: type: let baseName = baseNameOf path; in
        ! (baseName == ".git" || baseName == "run" || baseName == "result" ||
           baseName == "BLAH" || lib.hasSuffix "~" baseName))
      ./..;

    #src = fetchFromGitLab { owner = "mukn"; repo = "glow";
    #  rev = "dbf1a33e67e5ea97456f3c1ee9bf22c994623306";
    #  sha256 = "106zp5131rq9ry8c2axxf5wfjg4sm2q99laqnzd5g5hk5bimi2az"; };
    meta = {
      description = "Glow: language for safe Decentralized Applications (DApps)";
      homepage    = "https://glow-lang.org";
      license     = lib.licenses.asl20;
      platforms   = lib.platforms.unix;
      maintainers = with lib.maintainers; [ fare ];
    };
  };
}
