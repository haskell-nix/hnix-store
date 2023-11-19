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
  # srk 2023-11-18: this is pinned in nixpkgs unstable to 1.0.5
  # causing trouble with ghc963
  some = hself.callHackageDirect
    { pkg = "some";
      ver = "1.0.6";
      sha256 = "sha256-AnjaUzSlsLi3lIURrEfs92Jo5FzX49RyNdfDSfFV3Kk=";
    } {};

  # srk 2023-11-19: default in unstable is 0.1.1.1 which
  # fails to compile test on ghc8107
  # but for for ghc963 we hit
  # https://github.com/obsidiansystems/dependent-sum-template/issues/10
  # so we use 0.1.1.1 for ghc963 and 0.2.0.0 for the rest
  # - some weird interaction in unstable as this builds
  # with cabal and 0.2.0.0
  dependent-sum-template =
    if compiler == "ghc963"
    then hsuper.dependent-sum-template
    else hsuper.dependent-sum-template_0_2_0_0;

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
}
