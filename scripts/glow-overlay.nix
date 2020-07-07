# You can symlink this overlay into your ~/.config/nixpkgs/overlays/
# to make glow-unstable available to your nix-env -iA glow-unstable.
pkgs: super: rec {
  inherit (super) lib fetchFromGitLab fetchgit
     gerbil-unstable gambit-support gerbil-support gerbilPackages-unstable;

  muknglow = gerbil-support.gerbilPackage {
    pname = "muknglow";
    version = "unstable-2020-05-02";
    git-version = "0.0-207-gd0ad110";
    package = "glow";
    gerbil = gerbil-unstable;
    gambit-params = gambit-support.unstable-params;
    gerbilInputs = [gerbilPackages-unstable.gerbil-utils];
    version-path = "config/version.ss";
    src = ./..;
    #src = fetchFromGitLab { owner = "mukn"; repo = "glow";
    #  rev = "d0ad110a22b622844a4b916326b221ed3bbe66c4";
    #  sha256 = "1xyslg5maz60wxrc4jqndx8czfi8b2b9f0n0rsm00000000giddf"; };
    meta = {
      description = "Decentralized Application Language";
      homepage    = "https://gitlab.com/mukn/glow";
      license     = lib.licenses.asl20;
      platforms   = lib.platforms.unix;
      maintainers = with lib.maintainers; [ fare ];
    };
  };
}
