set -e
set -x

rm -rf dist-newstyle
cabal test hnix-store-remote-tests

nix-build -E 'with (import ./. {}); pkgs.haskell.lib.buildFromSdist haskellPackages.hnix-store-core'
nix-build -E 'with (import ./. {}); pkgs.haskell.lib.buildFromSdist haskellPackages.hnix-store-remote'

