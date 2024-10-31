{ pkgs ? import <nixpkgs> {}
, compiler ? null
}:
let
  lib = pkgs.lib;
  overlay = import ./overlay.nix pkgs compiler;
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
  inherit (haskellPackages)
    nix-derivation
    hnix-store-core
    hnix-store-db
    hnix-store-json
    hnix-store-nar
    hnix-store-readonly
    hnix-store-remote
    hnix-store-tests;
  haskellPackages = lib.dontRecurseIntoAttrs haskellPackages;
  pkgs = lib.dontRecurseIntoAttrs pkgs;
}
