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
  # srk 2023-12-06: until in unstable
  dependent-sum-template = hself.callHackageDirect
    { pkg = "dependent-sum-template";
      ver = "0.2.0.1";
      sha256 = "sha256-quwgFuEBrK96JZenJZcyfk/O0Gp+ukwKEpe1hMqDbIg=";
    } {};

  # srk 2023-11-19: wider unix bound via CPP
  # Required for ghc963 since linux-namespaces is pinned
  # in unstable to 0.1.3.0
  linux-namespaces = hself.callCabal2nix "linux-namespaces"
    (fetchGitHubPR {
      url = "https://github.com/redneb/hs-linux-namespaces/pull/4";
      sha256 = "sha256-R61OCu6b4YoDzIl0vg8cSoP7611TlEdWZfVDnZrJY+g=";
    }) {};

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
