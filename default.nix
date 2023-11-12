{ pkgs ? import <nixpkgs> {}
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
  haskellPackages =
    pkgs.haskellPackages.override overrideHaskellPackages;
in {
  inherit (haskellPackages) hnix-store-core hnix-store-remote;
  inherit haskellPackages;
  inherit pkgs;
}
