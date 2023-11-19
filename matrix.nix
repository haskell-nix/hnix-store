# Matrix build for all packages and compilers
#
# Uses ./.github/workflows/ci.dhall as a single source of
# truth for a list of supported compilers
#
# The dhall expression needs to be frozen via dhall freeze
# which is done automatically when we update it via ./.github/workflows/ci.sh
# and dhallDirectoryToNix uses ci.dhall.frozen instead
{ pkgs ? import <nixpkgs> {}
}:
let
  lib = pkgs.lib;
  ciDhallNix = pkgs.dhallDirectoryToNix { src = ./.github/workflows; file = "ci.dhall.frozen"; };
  ciCompilers = ciDhallNix.jobs.build.strategy.matrix.ghc;

  # from e.g. 9.6.3 to ghc963
  convertCompilers = cs:
    map (x: "ghc${lib.strings.replaceStrings ["."] [""] x}") cs;

  compilers =
    lib.traceValFn (cs:
      let
        prettyCs = lib.concatMapStringsSep "\n" (c: "- ${c}") cs;
      in
        "building for compilers:\n${prettyCs}")
      (convertCompilers ciCompilers);
in
pkgs.lib.recurseIntoAttrs
  (pkgs.lib.genAttrs compilers (compiler:
    pkgs.lib.recurseIntoAttrs
      (import ./. { inherit pkgs compiler; })
    )
  )
