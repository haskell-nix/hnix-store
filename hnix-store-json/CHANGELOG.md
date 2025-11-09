# Unreleased

The test suite is now much more comprenesive, uses test data from upstream Nix to make sure the JSON formats match.

   * Comprehensive test suite with upstream Nix test data
* Additions:
   * JSON instances for `StorePath`, `Hash`, `ContentAddress`
   * JSON instances for `OutputsSpec`, `SingleDerivedPath`, `DerivedPath`
   * JSON instances for `BuildTraceKey`, `Derivation`, `FreeformDerivationOutput`
   * Roundtrip property tests for all JSON-serializable types

# 0.1.0.0 2024-07-31

* Initial release

---

`hnix-store-json` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
