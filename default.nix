{ pkgs ? import <nixpkgs> {} }: let
  overlay = import ./overlay.nix;
  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then pkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };
in {
  haskellPackages =
    pkgs.haskellPackages.override overrideHaskellPackages;
  haskell865Packages =
    pkgs.haskell.packages.ghc865.override overrideHaskellPackages;
  inherit pkgs;
}
