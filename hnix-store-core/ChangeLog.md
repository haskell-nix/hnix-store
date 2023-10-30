# ChangeLog

## Unreleased 202y-mm-dd

* Breaking:
    * [(link)](https://github.com/haskell-nix/hnix-store/pull/216) `StorePath` no longer carries `storePathRoot` field and we
      have a stand-alone `StoreDir` type instead to be used instead of `FilePath`
      when store root directory is needed as a context.

## [0.6.1.0](https://github.com/haskell-nix/hnix-store/compare/core-0.6.0.0...core-0.6.1.0) 2023-01-02

* Fixed:

    * [(link)](https://github.com/haskell-nix/hnix-store/pull/201) [(link)](https://github.com/haskell-nix/hnix-store/pull/203) NAR serialization compatibility (symlinks, directory symlinks, UTF-8 handling)

## [0.6.0.0](https://github.com/haskell-nix/hnix-store/compare/core-0.5.0.0...core-0.6.0.0) 2022-06-06

* Breaking:

    * [(link)](https://github.com/haskell-nix/hnix-store/pull/177) `streamNarIO` changes type and returns `NarSource m`
      * `FilePath` can turn to `NarSource m` using `dumpPath`
      * `ByteString` can turn to `NarSource m` using `dumpString`

## [0.5.0.0](https://github.com/haskell-nix/hnix-store/compare/0.4.3.0...core-0.5.0.0) 2021-06-10

* Breaking:

  * `System.Nix.Hash`:
    * [(link)](https://github.com/haskell-nix/hnix-store/pull/157/commits/97146b41cc87327625e02b81971aeb2fd7d66a3f) Migration from packages `cryptohash-` -> `cryptonite`:
      * rm `newtype Digest` in favour of `cryptonite: newtype Digest`
      * rm `data HashAlgorithm` in favour of `cryptonite: class HashAlgorithm`
      * rm `class ValidAlgo` in favour of `cryptonite: class HashAlgorithm`.
      * `class NamedAlgo` removed `hashSize` in favour of `cryptonite: class HashAlgorithm: hashDigestSize`. Former became a subclass of the latter.
      * rm `hash` in favour of `cryptonite: hash`
      * rm `hashLazy` in favour of `cryptonite: hashlazy`
    * [(link)](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3) Base encoding/decoding function for hashes (digests) changed (due to changes in type system & separation of specially truncated Nix Store hasing):
      * `encode(InBase -> DigestWith)`
      * `decode(Base -> DigestWith)`
  * [(link)](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3) `System.Nix.StorePath`:
    * rm `type StorePathHashAlgo = 'Truncated 20 'SHA256` in favour of `StorePathHashPart` & `mkStorePathHashPart`.
    * rm `unStorePathName`, please use `GHC: coerce` for `StorePathName <-> Text`, `StorePathName` data constructor is provided.
  * `Internal` modules now have export lists, if something, please contact.


* Additional:

  * [(link)](https://github.com/haskell-nix/hnix-store/pull/157/commits/97146b41cc87327625e02b81971aeb2fd7d66a3f) Support of GHC 9.0.

  * [(link)](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3) `System.Nix.StorePath`:
    * exposed `StorePathName` data constructor to API.
    * added `newtype StorePathHashPart = StorePathHashPart ByteString`.
      * added builder `mkStorePathHashPart :: ByteString -> StorePathHashPart`
  * [(link)](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3) `System.Nix.Hash`:
    * Nix store (which are specially truncated) hashes are now handled separately from other hashes:
      * add `mkStorePathHash` - a function to create a content into Nix storepath-style hash:
        `mkStorePathHash :: HashAlgorithm a => ByteString -> ByteString`
        but recommend to at once use `mkStorePathHashPart`.

## [0.4.3.0](https://github.com/haskell-nix/hnix-store/compare/0.4.2.0...0.4.3.0) 2021-05-30

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix-store/commit/b85f7c875fe6b0bca939ffbcd8b9bd0ab1598aa0) `System.Nix.ReadonlyStore`: add a readonly `computeStorePathForPath`
  * [(link)](https://github.com/haskell-nix/hnix-store/commit/db71ecea3109c0ba270fa98a9041a8556e35217f) `System.Nix.ReadonlyStore`: `computeStorePathForPath`: force SHA256 as it's the only valid choice
  * [(link)](https://github.com/haskell-nix/hnix-store/commit/5fddf3c66ba1bcabb72c4d6b6e09fb41a7acd62c): `makeTextPath`: order the references

## [0.4.2.0](https://github.com/haskell-nix/hnix-store/compare/0.4.1.0...0.4.2.0) 2021-03-12

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
