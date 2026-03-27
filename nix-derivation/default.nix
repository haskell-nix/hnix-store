# You can build this repository using Nix by running:
#
#     $ nix-build
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          nix-derivation =
            pkgs.haskell.lib.overrideCabal
              (haskellPackagesNew.callCabal2nix "nix-derivation" ./. { })
              (_: {
#                 withBenchmarkDepends = true;
                }
              );
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { nix-derivation = pkgs.haskellPackages.nix-derivation;
  }
