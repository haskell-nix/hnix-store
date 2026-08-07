{ pkgs ? (import ../../default.nix {}).pkgs }:

let
  narShell = (import ../../shell.nix { inherit pkgs; }).hnix-store-nar;
in
pkgs.mkShell {
  inputsFrom = [ narShell ];
  packages = [
    pkgs.cabal-install
    pkgs.hyperfine
    pkgs.util-linux
  ];
}
