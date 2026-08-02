{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/148bab9c1c3c53136ecb44a6ea356a0ed5b39b06"; # nixos-unstable
    sha256 = "130q3prp2m6863lzc7rhv6ak42g1xr4hhpn7mccp979aqk4fr11a";
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
    hnix-store-aterm
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
