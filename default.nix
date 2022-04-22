{ rev ?  "c542baa0c894796c92a8173dead027f3b952c22e"
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
