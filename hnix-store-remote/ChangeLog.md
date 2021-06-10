# Revision history for hnix-store-remote

## [0.5.0.0](https://github.com/haskell-nix/hnix-store/compare/0.4.3.0...0.5.0.0) 2021-06-11

* Breaking:
  * [(link)](https://github.com/haskell-nix/hnix-store/commit/3b948d112aa9307b0451258f28c7ee5dc86b24c7) `System.Nix.Store.Remote`:
    * `addToStore`: constraint of `ValidAlgo a` removed in favour of constraint on `cryptonite: HashAlgorithm a` through constraint `NamedAlgo a`.
    * `queryPathFromHashPart`: 1st arg changed from `Digest StorePathHashAlgo` to `StorePathHashPart`, for details: [hnix-store-core 0.5.0.0 ChangeLog](https://hackage.haskell.org/package/hnix-store-core-0.5.0.0/changelog).

## [0.4.3.0](https://github.com/haskell-nix/hnix-store/compare/0.4.2.0...0.4.3.0) 2021-05-30

Nothing (it is tandem `hnix-store-core` fix release)

## [0.4.2.0](https://github.com/haskell-nix/hnix-store/compare/0.4.1.0...0.4.2.0) 2021-03-12

* Additional:

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/5d03ffc43cde9448df05e84838ece70cc83b1b6c) Cabal now properly states `tasty-discover` as `build-tool-depends`.

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/b5ad38573d27e0732d0fadfebd98de1f753b4f07) added explicit `hie.yml` cradle description for `cabal` to help Haskell Language Server to work with monorepo.

  * [(link)](https://github.com/haskell-nix/hnix-store/commit/cf04083aba98ad40d183d1e26251101816cc07ae) Nix dev env: removed GHC 8.6.5 support, afaik it is not even in Nixpkgs anymore.


## [0.4.1.0](https://github.com/haskell-nix/hnix-store/compare/0.4.0.0...0.4.1.0) 2021-01-16

* `System.Nix.Store.Remote`: module API now re-exports `System.Nix.Store.Remote.Types` API
* Big clean-up of dependencies.

## [0.4.0.0](https://github.com/haskell-nix/hnix-store/compare/0.3.0.0...0.4.0.0) 2020-12-30

* `hnix-store-core` compatibility

## 0.3.0.0 -- 2020-11-29

* Restored most store API functions except `addToStoreNar`
* Added `buildDerivation`

## 0.2.0.0 -- skipped

* `hnix-store-core` release only

## 0.1.0.0  -- 2019-03-18

* First version.
