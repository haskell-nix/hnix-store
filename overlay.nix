pkgs: hlib: helf: huper:
let
  lib = pkgs.lib;
in
{
  # srk 2023-11-18: this is pinned in nixpkgs unstable to 1.0.5
  # causing trouble with ghc963
  some = helf.callHackageDirect
    { pkg = "some";
      ver = "1.0.6";
      sha256 = "sha256-AnjaUzSlsLi3lIURrEfs92Jo5FzX49RyNdfDSfFV3Kk=";
    } {};

  hnix-store-core =
    lib.pipe
      (helf.callCabal2nix "hnix-store-core" ./hnix-store-core {})
      [
        hlib.compose.buildFromSdist
      ];
  hnix-store-remote =
    lib.pipe
      # enable -fio-testsuite for Linux systems as
      # it requires linux-namespaces
      # NOTE: we cannot use hlib.compose.enableCabalFlag
      # as the testsuite deps won't get picked up
      # after cabal2nix step
      (
      if pkgs.stdenv.isDarwin
      then helf.callCabal2nix "hnix-store-remote" ./hnix-store-remote {}
      else helf.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote "-fio-testsuite" {}
      )
      [
        hlib.compose.buildFromSdist
        (pkg: pkg.overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ [
            pkgs.nix
          ];
        }))
      ];
}
