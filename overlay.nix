pkgs: compiler: hself: hsuper:
let
  lib = pkgs.lib;
  haskellLib = pkgs.haskell.lib;

  fetchGitHubPR = { url, sha256 ? throw "sha256 required", ...}@x:
    let
      m = builtins.match "https://github.com/(.+)/(.+)/pull/([0-9]+)" url;
      parts = if m != null then m else throw "Failed to match PR URL";
    in
      pkgs.fetchFromGitHub ({
        owner  = builtins.elemAt parts 0;
        repo   = builtins.elemAt parts 1;
        rev    = "refs/pull/${builtins.elemAt parts 2}/head";
      } // (lib.filterAttrs (n: v: n != "url") x));
in
{
  hnix-store-core =
    lib.pipe
      (hself.callCabal2nix "hnix-store-core" ./hnix-store-core/hnix-store-core.cabal {})
      [
        (drv: drv.overrideAttrs (old: { src = ./hnix-store-core; }))
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-db =
    lib.pipe
      (hself.callCabal2nix "hnix-store-db" ./hnix-store-db/hnix-store-db.cabal {})
      [
        (drv: drv.overrideAttrs (old: { src = ./hnix-store-db; }))
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-aterm =
    lib.pipe
      (hself.callCabal2nix "hnix-store-aterm" ./hnix-store-aterm {})
      [
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-json =
    let
      # Include the JSON test data files from upstream Nix that we need
      # for testing.
      src = lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.unions [
          ./hnix-store-json
          (lib.fileset.fileFilter (file: file.hasExt "json") ./upstream-nix/src/libstore-tests/data/content-address)
          (lib.fileset.fileFilter (file: file.hasExt "json") ./upstream-nix/src/libstore-tests/data/derived-path)
          (lib.fileset.fileFilter (file: file.hasExt "json") ./upstream-nix/src/libstore-tests/data/outputs-spec)
          (lib.fileset.fileFilter (file: file.hasExt "json") ./upstream-nix/src/libstore-tests/data/realisation)
          (lib.fileset.fileFilter (file: file.hasExt "json") ./upstream-nix/src/libstore-tests/data/store-path)
          (lib.fileset.fileFilter (file: file.hasExt "json") ./upstream-nix/src/libutil-tests/data/hash)
        ];
      };
    in
    lib.pipe
      (hself.callCabal2nix "hnix-store-json" ./hnix-store-json/hnix-store-json.cabal {})
      [
        (drv: drv.overrideAttrs (old: {
          inherit src;
          # Set up symlinks to upstream data files
          postUnpack = ''
            sourceRoot+=/hnix-store-json
          '';
        }))
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-nar =
    lib.pipe
      (hself.callCabal2nix "hnix-store-nar" ./hnix-store-nar/hnix-store-nar.cabal {})
      [
        (drv: drv.overrideAttrs (old: { src = ./hnix-store-nar; }))
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-readonly =
    lib.pipe
      (hself.callCabal2nix "hnix-store-readonly" ./hnix-store-readonly/hnix-store-readonly.cabal {})
      [
        (drv: drv.overrideAttrs (old: { src = ./hnix-store-readonly; }))
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-remote =
    lib.pipe
      # enable -fio-testsuite for Linux systems as
      # it requires linux-namespaces
      # NOTE: we cannot use haskellLib.compose.enableCabalFlag
      # as the testsuite deps won't get picked up
      # after cabal2nix step
      (
      if pkgs.stdenv.isDarwin
      then hself.callCabal2nix "hnix-store-remote" ./hnix-store-remote/hnix-store-remote.cabal {}
      else hself.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote/hnix-store-remote.cabal "-fio-testsuite" {}
      )
      [
        (drv: drv.overrideAttrs (old: { src = ./hnix-store-remote; }))
        haskellLib.compose.buildFromSdist
        (pkg: pkg.overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ [
            pkgs.nix
          ];
        }))
      ];
  hnix-store-tests =
    lib.pipe
      (hself.callCabal2nix "hnix-store-tests" ./hnix-store-tests/hnix-store-tests.cabal {})
      [
        (drv: drv.overrideAttrs (old: { src = ./hnix-store-tests; }))
        haskellLib.compose.buildFromSdist
      ];
}
