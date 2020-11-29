# Revision history for hnix-store-core

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
