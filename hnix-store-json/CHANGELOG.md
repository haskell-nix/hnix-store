# Unreleased

The test suite is now much more comprenesive, uses test data from upstream Nix to make sure the JSON formats match.

* JSON instance for `StorePath` now doesn't include the store dir, matching upstream.
  (This keeps it canonical.)

* Removed `deriving-aeson` and `constraints-extras` dependencies; all JSON instances are now hand-written.

* New JSON instances for:
  * `Hash`
  * `ContentAddress`
  * `OutputsSpec`
  * `SingleDerivedPath`
  * `DerivedPath`
  * `Signature`
  * `NamedSignature`
  * `BuildTraceKey`
  * `Realisation`
  * `RealisationWithId`
  * `BuildSuccessStatus`, `BuildFailureStatus`
  * `BuildResult`
  * `Metadata StorePath` (path-info version 3 format)

# 0.1.0.0 2024-07-31

* Initial release

---

`hnix-store-json` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
