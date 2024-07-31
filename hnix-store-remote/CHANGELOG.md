# [0.7.0.0](https://github.com/haskell-nix/hnix-store/compare/remote-0.6.0.0...remote-0.7.0.0) 2024-07-31

* Changes:
    * `StorePath` no longer carries `storePathRoot` field and we
      have a stand-alone `StoreDir` type instead to be used instead of `FilePath`
      when store root directory is needed as a context.
      Fore `-remote`, this affects `runStoreOpts` and its variants [#216](https://github.com/haskell-nix/hnix-store/pull/216)
    * The old `MonadStore` is now deprecated and aliased to `RemoteStoreT IO`
    * All store operations now use `MonadRemoteStore` typeclass, which `RemoteStoreT` is an instance of
    * Couple of `Bool` parameters switched to enums

* Additions
    * `addToStoreNAR` store operation [#277](https://github.com/haskell-nix/hnix-store/pull/277)
    * `narFromPath` store operation [#279](https://github.com/haskell-nix/hnix-store/pull/279)
    * Initial server-side support with proxy daemon acting as MITM proxy,
      which allows for testing of both sides of the remote store protocol

The library stability is not quite there yet and should be considered experimental.

# [0.6.0.0](https://github.com/haskell-nix/hnix-store/compare/remote-0.5.0.0...remote-0.6.0.0) 2021-06-06

* Changes:
  * `System.Nix.Store.Remote` [#179](https://github.com/haskell-nix/hnix-store/pull/179)
    * `addToStore` no longer accepts `FilePath` as its second argument but uses
      more generic `NarSource` [(NarSource PR)](https://github.com/haskell-nix/hnix-store/pull/177)

# [0.5.0.0](https://github.com/haskell-nix/hnix-store/compare/0.4.3.0...remote-0.5.0.0) 2021-06-11

* Changes:
  * `System.Nix.Store.Remote` [#161](https://github.com/haskell-nix/hnix-store/pull/161)
    * `addToStore`: constraint of `ValidAlgo a` removed in favour of constraint on `cryptonite: HashAlgorithm a` through constraint `NamedAlgo a`.
    * `queryPathFromHashPart`: 1st arg changed from `Digest StorePathHashAlgo` to `StorePathHashPart`, for details: [hnix-store-core 0.5.0.0 ChangeLog](https://hackage.haskell.org/package/hnix-store-core-0.5.0.0/changelog).

# [0.4.3.0](https://github.com/haskell-nix/hnix-store/compare/0.4.2.0...0.4.3.0) 2021-05-30

Nothing (it is tandem `hnix-store-core` fix release)

# [0.4.2.0](https://github.com/haskell-nix/hnix-store/compare/0.4.1.0...0.4.2.0) 2021-03-12

* Additions:
  * Cabal now properly states `tasty-discover` as `build-tool-depends` [#130](https://github.com/haskell-nix/hnix-store/pull/130)
  * added explicit `hie.yml` cradle description for `cabal` to help Haskell Language Server to work with monorepo. [#132](https://github.com/haskell-nix/hnix-store/pull/132)
  * Nix dev env: removed GHC 8.6.5 support, afaik it is not even in Nixpkgs anymore [#136](https://github.com/haskell-nix/hnix-store/pull/136)

# [0.4.1.0](https://github.com/haskell-nix/hnix-store/compare/0.4.0.0...0.4.1.0) 2021-01-16

* `System.Nix.Store.Remote`: module API now re-exports `System.Nix.Store.Remote.Types` API
* Big clean-up of dependencies.

# [0.4.0.0](https://github.com/haskell-nix/hnix-store/compare/0.3.0.0...0.4.0.0) 2020-12-30

* `hnix-store-core` compatibility

# 0.3.0.0 -- 2020-11-29

* Restored most store API functions except `addToStoreNar`
* Added `buildDerivation`

# 0.2.0.0 -- skipped

* `hnix-store-core` release only

# 0.1.0.0  -- 2019-03-18

* First version.

---

`hnix-store-remote` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
