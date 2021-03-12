# Revision history for hnix-store-core

## [next](https://github.com/haskell-nix/hnix-store/compare/0.4.1.0...0.4.2.0) 2021-03-12

* Additional:

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/5d03ffc43cde9448df05e84838ece70cc83b1b6c) Cabal now properly states `tasty-discover` as `build-tool-depends`.

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/b5ad38573d27e0732d0fadfebd98de1f753b4f07) added explicit `hie.yml` cradle description for `cabal` to help Haskell Language Server to work with monorepo.

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/a5b7a614c0e0e11147a93b9a197c2a443afa3244) rm vacuous `Setup.hs`, it was throwing-off HLS, and anyway file is vacuous and gets deprecated by Cabal itself.

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/cf04083aba98ad40d183d1e26251101816cc07ae) Nix dev env: removed GHC 8.6.5 support, afaik it is not even in Nixpkgs anymore.

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/2a897ab581c0501587ce04da6d6e3a6f543b1d72) Test suite: fixed nar test for the envs without `/proc` (test suite now works on `macOS`).


## [0.4.1.0](https://github.com/haskell-nix/hnix-store/compare/0.4.0.0...0.4.1.0) 2021-01-16

* Big clean-up of dependencies.

## [0.4.0.0](https://github.com/haskell-nix/hnix-store/compare/0.3.0.0...0.4.0.0) 2020-12-30

* `System.Nix.Hash` no longer exports `encodeBase16, decodeBase16` and their `Base32` counterparts.
    These were replaced by `encodeInBase` and `decodeBase` functions
    accepting `BaseEncoding` data type [#87](https://github.com/haskell-nix/hnix-store/pull/87)
* Support `base16-bytestring >= 1` [#86](https://github.com/haskell-nix/hnix-store/pull/86) [#100](https://github.com/haskell-nix/hnix-store/pull/100)

## 0.3.0.0 -- 2020-11-29

* `System.Nix.Nar` changes API to support NAR format streaming:
  * `buildNarIO :: FilePath -> Handle -> IO ()` - Create a NAR from a regular filesystem object, stream it out on the Handle
  * `unpackNarIO :: Handle -> FilePath -> IO ()` - Recreate filesystem object from a NAR file accessed by the Handle
* `StorePath` type changed to simple variant without type level
symbolic store path root.
* Added `makeFixedOutputPath` to `System.Nix.ReadonlyStore`
* Added `decodeBase16` and `decodeBase32` to `System.Nix.Hash`
* `System.Nix.StorePath` module now provides
  * `storePathToFilePath` and `storePathToText` helpers
  * `storePathToNarInfo` for converting paths to `narinfo` URLs
  * `parsePath` function
  * `pathParser` Attoparsec parser
* Added `System.Nix.Build` module
* Added `System.Nix.Derivation` module
* Removed `System.Nix.Util` module, moved to `hnix-store-remote`
* Added base64 and SHA512 hash support

## 0.2.0.0 -- 2020-03-12

Removed `System.Nix.Store`. We may reintroduce it later when multiple backends
exist and we can tell what common effects they should share.

## 0.1.0.0  -- 2019-03-18

* First version.
