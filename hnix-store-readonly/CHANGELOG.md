# [next](https://github.com/haskell-nix/hnix-store/compare/readonly-0.2.0.0...master) 2026-MM-DD

# [0.2.0.0](https://github.com/haskell-nix/hnix-store/compare/readonly-0.1.0.0...readonly-0.2.0.0) 2026-10-21

Remove duplicate code from `hnix-store-readonly`

`References`, `makeStorePath`, `makeFixedOutputPath`, and `makeType`
already exist in `System.Nix.StorePath.ContentAddressed` (added in
cf6c416) but were never removed from `System.Nix.Store.ReadOnly`.

- Remove the redundant definitions from `hnix-store-readonly`
- Move tests for `makeStorePath`/`makeFixedOutputPath` to
  `hnix-store-core` where the code now lives
- Remove unused dependencies from `hnix-store-readonly`

# 0.1.0.0 2024-07-31

* Initial release

---

`hnix-store-readonly` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
