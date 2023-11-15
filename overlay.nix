pkgs: hlib: helf: huper:
let
  lib = pkgs.lib;
in
{
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
