{ pkgs ? import <nixpkgs> {} }: let
  overlay = import ./overlay.nix pkgs pkgs.haskell.lib;
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
  inherit pkgs;
}
