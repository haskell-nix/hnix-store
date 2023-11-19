{ pkgs ? import <nixpkgs> {}
, compiler ? null
}:
let
  overlay = import ./overlay.nix pkgs pkgs.haskell.lib;
  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then pkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };

  packageSet =
    if compiler == null
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = packageSet.override overrideHaskellPackages;
in {
  inherit (haskellPackages) hnix-store-core hnix-store-remote;
  inherit haskellPackages;
  inherit pkgs;
}
