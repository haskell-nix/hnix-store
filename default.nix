{ rev ?  "272fad732d39b24c4549c475176e0d8cbc8c897a"
, pkgs ?
    if ((rev == "") || (rev == "default") || (rev == "local"))
      then import <nixpkgs> {}
      # Do not guard with hash, so the project is able to use current channels (rolling `rev`) of Nixpkgs
      else import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {}
    // {
      # Try to build dependencies even if they are marked broken.
      config.allowBroken = true;
    }
}: let
  overlay = import ./overlay.nix pkgs pkgs.haskell.lib;
  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then pkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };
in {
  haskellPackages =
    pkgs.haskellPackages.override overrideHaskellPackages;
  inherit pkgs;
}
