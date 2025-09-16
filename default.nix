{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/c23193b943c6c689d70ee98ce3128239ed9e32d1"; # nixos-unstable
    sha256 = "0vgk8mrprrh6w7zw2id3hc858kqw5pwdc52ma2f95rz36gchxcc4";
  }) {}
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
