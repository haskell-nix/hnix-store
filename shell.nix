let
  packages = [ "hnix-store-core" "hnix-store-remote" "hnix-store-s3" ];
  inherit (import ./. {}) pkgs haskellPackages;
  hslib = pkgs.haskell.lib;
  extract-external-inputs = p:
    builtins.filter (dep: !(builtins.elem dep packages))
      (map (x: x.pname) (hslib.getHaskellBuildInputs haskellPackages.${p}));
  external-inputs = map (x: haskellPackages.${x}) (builtins.concatLists
    (map extract-external-inputs packages));
  package-envs = builtins.listToAttrs (map (p: {
    name = p;
    value = haskellPackages.${p}.env;
  }) packages);
in (haskellPackages.mkDerivation {
  pname = "hnix-store-core";
  version = "0.0.0.0";
  libraryHaskellDepends = external-inputs;
  license = pkgs.stdenv.lib.licenses.asl20;
}).env // package-envs
