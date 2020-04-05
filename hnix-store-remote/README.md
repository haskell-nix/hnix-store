hnix-store-remote
=================

Nix worker protocol implementation for interacting with remote Nix store
via `nix-daemon`.

## API

[System.Nix.Store.Remote]: ./src/System/Nix/Store/Remote.hs

## Example

```haskell
import System.Nix.Store.Remote

main =
  runStore_ $ do
    syncWithGC
    optimiseStore
```
