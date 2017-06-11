let pkgs = import <nixpkgs> {};

in {
  hnix-store = pkgs.haskellPackages.callPackage ./default.nix {};
}
