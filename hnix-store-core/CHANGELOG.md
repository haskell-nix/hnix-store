# Next

* Changes:
   * Constructors of `StorePathName` and `StorePathHashPart` are no longer
     exported. Use respective `mkStorePath..` functions. [#230](https://github.com/haskell-nix/hnix-store/pull/230)
   * `StorePathSet` type alias is no more, use `HashSet StorePath` [#230](https://github.com/haskell-nix/hnix-store/pull/230)
   * `makeStorePath` and `parsePath` now returns `Either InvalidPathError StorePath` [#231](https://github.com/haskell-nix/hnix-store/pull/231)
   * `BuildResult`s `timesBuild` field changes type from `Integer` to `Int` [#231](https://github.com/haskell-nix/hnix-store/pull/231)

* Additions:
   * `InvalidPathError` replacing previous stringy error [#231](https://github.com/haskell-nix/hnix-store/pull/231)
   * Added `Arbitrary` instances for (exported by default) [#230](https://github.com/haskell-nix/hnix-store/pull/230)
     * `StorePath`
     * `StorePathName`
     * `StorePathHashPart`
     * `StoreDir`
   * Added `Arbitrary` instances for [#231](https://github.com/haskell-nix/hnix-store/pull/231)
     * `BuildMode`
     * `BuildStatus`
     * `BuildResult`
     * `Derivation StorePath Text`
     * `DerivationOutput StorePath Text`

# [0.7.0.0](https://github.com/haskell-nix/hnix-store/compare/core-0.6.1.0...core-0.7.0.0) 2023-11-15

* Changes:
    * `StorePath` no longer carries `storePathRoot` field and we
      have a stand-alone `StoreDir` type instead to be used instead of `FilePath`
      when store root directory is needed as a context [#216](https://github.com/haskell-nix/hnix-store/pull/216)

* Fixes:
    * NAR encoding and decoding now supports case-insensitive filesystems [#218](https://github.com/haskell-nix/hnix-store/pull/218)
      * The "case hack" replicates the behavior of the `use-case-hack` option in Nix, which adds a suffix to conflicting filenames.
        This feature is enabled by default on macOS (darwin).

* Additions:
      * `data NarOptions` has been added to configure NAR encoding and decoding. The `optUseCaseHack` field can be used to enable or disable the case hack [#218](https://github.com/haskell-nix/hnix-store/pull/218)
      * New `streamNarIOWithOptions` and `runParserWithOptions` functions have been added to `System.Nix.Nar` to support the new configurable options [#218](https://github.com/haskell-nix/hnix-store/pull/218)

# [0.6.1.0](https://github.com/haskell-nix/hnix-store/compare/core-0.6.0.0...core-0.6.1.0) 2023-01-02

* Fixes:

    * NAR serialization compatibility (symlinks, directory symlinks, UTF-8 handling) [#201](https://github.com/haskell-nix/hnix-store/pull/201) [#203](https://github.com/haskell-nix/hnix-store/pull/203)

# [0.6.0.0](https://github.com/haskell-nix/hnix-store/compare/core-0.5.0.0...core-0.6.0.0) 2022-06-06

* Breaking:

    * `streamNarIO` changes type and returns `NarSource m` [#177](https://github.com/haskell-nix/hnix-store/pull/177)
      * `FilePath` can turn to `NarSource m` using `dumpPath`
      * `ByteString` can turn to `NarSource m` using `dumpString`

# [0.5.0.0](https://github.com/haskell-nix/hnix-store/compare/0.4.3.0...core-0.5.0.0) 2021-06-10

* Breaking:

  * `System.Nix.Hash`:
    * Migration from packages `cryptohash-` -> `cryptonite` [#157](https://github.com/haskell-nix/hnix-store/pull/157/commits/97146b41cc87327625e02b81971aeb2fd7d66a3f)
      * rm `newtype Digest` in favour of `cryptonite: newtype Digest`
      * rm `data HashAlgorithm` in favour of `cryptonite: class HashAlgorithm`
      * rm `class ValidAlgo` in favour of `cryptonite: class HashAlgorithm`.
      * `class NamedAlgo` removed `hashSize` in favour of `cryptonite: class HashAlgorithm: hashDigestSize`. Former became a subclass of the latter.
      * rm `hash` in favour of `cryptonite: hash`
      * rm `hashLazy` in favour of `cryptonite: hashlazy`
    * Base encoding/decoding function for hashes (digests) changed (due to changes in type system & separation of specially truncated Nix Store hasing) [#157](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3)
      * `encode(InBase -> DigestWith)`
      * `decode(Base -> DigestWith)`
  * `System.Nix.StorePath` [#157](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3)
    * rm `type StorePathHashAlgo = 'Truncated 20 'SHA256` in favour of `StorePathHashPart` & `mkStorePathHashPart`.
    * rm `unStorePathName`, please use `GHC: coerce` for `StorePathName <-> Text`, `StorePathName` data constructor is provided.
  * `Internal` modules now have export lists, if something, please contact.

* Additional:

  * Support of GHC 9.0 [#157](https://github.com/haskell-nix/hnix-store/pull/157/commits/97146b41cc87327625e02b81971aeb2fd7d66a3f)
  * `System.Nix.StorePath` [#157](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3)
    * exposed `StorePathName` data constructor to API.
    * added `newtype StorePathHashPart = StorePathHashPart ByteString`.
      * added builder `mkStorePathHashPart :: ByteString -> StorePathHashPart`
  * `System.Nix.Hash` [#157](https://github.com/haskell-nix/hnix-store/pull/157/commits/2af74986de8aef1a13dbfc955886f9935ca246a3)
    * Nix store (which are specially truncated) hashes are now handled separately from other hashes:
      * add `mkStorePathHash` - a function to create a content into Nix storepath-style hash:
        `mkStorePathHash :: HashAlgorithm a => ByteString -> ByteString`
        but recommend to at once use `mkStorePathHashPart`.

# [0.4.3.0](https://github.com/haskell-nix/hnix-store/compare/0.4.2.0...0.4.3.0) 2021-05-30

* Additional:
  * `System.Nix.ReadonlyStore`: add a readonly `computeStorePathForPath` [b85f7c8](https://github.com/haskell-nix/hnix-store/commit/b85f7c875fe6b0bca939ffbcd8b9bd0ab1598aa0)
  * `System.Nix.ReadonlyStore`: `computeStorePathForPath`: force SHA256 as it's the only valid choice [db71ece](https://github.com/haskell-nix/hnix-store/commit/db71ecea3109c0ba270fa98a9041a8556e35217f)
  * `makeTextPath`: order the references [5fddf3c](https://github.com/haskell-nix/hnix-store/commit/5fddf3c66ba1bcabb72c4d6b6e09fb41a7acd62c)

# [0.4.2.0](https://github.com/haskell-nix/hnix-store/compare/0.4.1.0...0.4.2.0) 2021-03-12

* Additional:
  * Cabal now properly states `tasty-discover` as `build-tool-depends` [5d03ffc](https://github.com/haskell-nix/hnix-store/commit/5d03ffc4cde9448df05e84838ece70cc83b1b6c) 
  * Added explicit `hie.yml` cradle description for `cabal` to help Haskell Language Server to work with monorepo [5bad385](https://github.com/haskell-nix/hnix-store/commit/b5ad38573d27e0732d0fadfebd98de1f753b4f07)
  * Removed vacuous `Setup.hs`, it was throwing-off HLS, and anyway file is vacuous and gets deprecated by Cabal itself [a5b7a61](https://github.com/haskell-nix/hnix-store/commit/a5b7a614c0e0e11147a93b9a197c2a443afa3244)
  * Nix dev env: removed GHC 8.6.5 support, afaik it is not even in Nixpkgs anymore [cf04083](https://github.com/haskell-nix/hnix-store/commit/cf04083aba98ad40d183d1e26251101816cc07ae)
  * Test suite: fixed nar test for the envs without `/proc` (test suite now works on `macOS`) [2a897ab](https://github.com/haskell-nix/hnix-store/commit/2a897ab581c0501587ce04da6d6e3a6f543b1d72)


# [0.4.1.0](https://github.com/haskell-nix/hnix-store/compare/0.4.0.0...0.4.1.0) 2021-01-16

* Big clean-up of dependencies.

# [0.4.0.0](https://github.com/haskell-nix/hnix-store/compare/0.3.0.0...0.4.0.0) 2020-12-30

* `System.Nix.Hash` no longer exports `encodeBase16, decodeBase16` and their `Base32` counterparts.
    These were replaced by `encodeInBase` and `decodeBase` functions
    accepting `BaseEncoding` data type [#87](https://github.com/haskell-nix/hnix-store/pull/87)
* Support `base16-bytestring >= 1` [#86](https://github.com/haskell-nix/hnix-store/pull/86) [#100](https://github.com/haskell-nix/hnix-store/pull/100)

# 0.3.0.0 -- 2020-11-29

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

# 0.2.0.0 -- 2020-03-12

Removed `System.Nix.Store`. We may reintroduce it later when multiple backends
exist and we can tell what common effects they should share.

# 0.1.0.0  -- 2019-03-18

* First version.
