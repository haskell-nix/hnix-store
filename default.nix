{
  compiler ? "ghc863"
, nixpkgs-rev ? "120eab94e0981758a1c928ff81229cd802053158"
, nixpkgs-sha256 ? "0qk6k8gxx5xlkyg05dljywj5wx5fvrc3dzp4v2h6ab83b7zwg813"

, pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgs-rev}.tar.gz";
    sha256 = nixpkgs-sha256;
  }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }

, doStrict  ? false
, doTracing ? false

}:

let

  overlay = self: super: builtins.mapAttrs (k: v: (

    pkgs.haskell.lib.overrideCabal v (drv: {

                   configureFlags =
                        pkgs.stdenv.lib.optional doTracing "--flags=tracing"
                     ++ pkgs.stdenv.lib.optional doStrict  "--ghc-options=-Werror";
      })

    )) (import ./overlay.nix self super);

  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then pkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };

in {
  haskellPackages =
    pkgs.haskell.packages.${compiler}.override overrideHaskellPackages;
  inherit pkgs;
  nixpkgs = pkgs;
}
