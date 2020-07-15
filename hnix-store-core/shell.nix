{
  compiler    ? "ghc883"

, useCabalName ? false
, cabalName ? "replace"
, packageRoot ? pkgs.nix-gitignore.gitignoreSource [ ] ./.

, allowInconsistentDependencies ? false
, doJailbreak ? false
, doCheck     ? true

, sdistTarball ? false
, buildFromSdist ? true

, failOnAllWarnings ? false
, buildStrictly ? false

, enableDeadCodeElimination ? false
, disableOptimization ? true
, linkWithGold ? false

, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, doTracing   ? false
, enableDWARFDebugging ? true
, doStrip ? false

, enableSharedLibraries ? true
, enableStaticLibraries ? false
, enableSharedExecutables ? false
, justStaticExecutables ? false
, enableSeparateBinOutput ? false

, checkUnusedPackages ? false
, doHaddock   ? false
, doHyperlinkSource ? false
, doCoverage  ? false
, doBenchmark ? false
, generateOptparseApplicativeCompletions ? false
, executableNamesToShellComplete ? [ "executableToComplete" ]


, withHoogle  ? true


, useRev ? false
, rev ? "nixpkgs-unstable"

, pkgs ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "Requires Nix >= 2.0"
    else
      if useRev
        # Do not guard with hash, so the project is able to use current channels (rolling `rev`) of Nixpkgs
        then import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {}
        else import <nixpkgs> {}
      // {
        # Try to build dependencies even if they are marked broken.
        config.allowBroken = true;
      }

, mkDerivation   ? null
}:

(import ../shell.nix).hnix-store-core
