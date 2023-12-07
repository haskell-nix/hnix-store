attrs@{...}:
let
  inherit (import ./. attrs) pkgs haskellPackages;
  hlib = pkgs.haskell.lib;

  packages = [
    "hnix-store-core"
    "hnix-store-db"
    "hnix-store-json"
    "hnix-store-nar"
    "hnix-store-readonly"
    "hnix-store-remote"
    "hnix-store-tests"
  ];
  extract-external-inputs = p:
    builtins.filter
      (dep: !(builtins.elem dep packages))
      (map
        (x: x.pname)
        (hlib.getHaskellBuildInputs haskellPackages.${p}));
  external-inputs =
    map
      (x: haskellPackages.${x})
      (builtins.concatLists
        (map
          extract-external-inputs
          packages));
  metaPackage =
    haskellPackages.mkDerivation
      { pname = "hnix-store-shell";
        version = "0.0.0.0";
        libraryHaskellDepends = external-inputs;
        license = pkgs.stdenv.lib.licenses.asl20;};

  package-envs =
    builtins.listToAttrs
      (map
        (p:
          { name = p;
            value = haskellPackages.${p}.env;})
        packages);

in

metaPackage.env // package-envs
