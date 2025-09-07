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
lib.optionalAttrs (hsuper ? dependent-sum-template_0_2_0_1) {
  # srk 2024-07-28: allow template-haskell 2.22 (GHC 9.8)
  # https://github.com/obsidiansystems/dependent-sum-template/pull/13
  dependent-sum-template =
    haskellLib.doJailbreak
      hsuper.dependent-sum-template_0_2_0_1;
} // {
  hnix-store-core =
    lib.pipe
      (hself.callCabal2nix "hnix-store-core" ./hnix-store-core {})
      [
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-db =
    lib.pipe
      (hself.callCabal2nix "hnix-store-db" ./hnix-store-db {})
      [
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-aterm =
    lib.pipe
      (hself.callCabal2nix "hnix-store-aterm" ./hnix-store-aterm {})
      [
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-json =
    lib.pipe
      (hself.callCabal2nix "hnix-store-json" ./hnix-store-json {})
      [
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-nar =
    lib.pipe
      (hself.callCabal2nix "hnix-store-nar" ./hnix-store-nar {})
      [
        haskellLib.compose.buildFromSdist
      ];
  hnix-store-readonly =
    lib.pipe
      (hself.callCabal2nix "hnix-store-readonly" ./hnix-store-readonly {})
      [
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
      then hself.callCabal2nix "hnix-store-remote" ./hnix-store-remote {}
      else hself.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote "-fio-testsuite" {}
      )
      [
        haskellLib.compose.buildFromSdist
        (pkg: pkg.overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ [
            pkgs.nix
          ];
        }))
      ];
  hnix-store-tests =
    lib.pipe
      (hself.callCabal2nix "hnix-store-tests" ./hnix-store-tests {})
      [
        haskellLib.compose.buildFromSdist
      ];
}
