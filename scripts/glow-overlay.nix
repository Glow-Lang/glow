# You can symlink this overlay into your ~/.config/nixpkgs/overlays/
# to make glow-lang available to your nix-env -iA glow-lang.
pkgs: super:
let inherit (super) lib fetchFromGitLab fetchgit gerbil-support;
    inherit (lib) lists;
    inherit (gerbil-support) overrideGerbilPackage gerbilFilterSource;
    maybeOverrideDep = name:
      let path = ../dep + "/" + name; in
      overrideGerbilPackage name
      (_: pkgs.thunkSource path) (builtins.fromJSON (builtins.readFile ./github.json)); in
  lists.foldr (f: x: f x) pkgs [
    (maybeOverrideDep "gerbil-utils")
    (maybeOverrideDep "gerbil-poo")
    (maybeOverrideDep "gerbil-ethereum")
    (overrideGerbilPackage "glow-lang" (_: gerbilFilterSource ./..))]
