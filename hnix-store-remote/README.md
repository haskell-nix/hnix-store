hnix-store-remote
=================

Nix worker protocol implementation for interacting with remote Nix store
via `nix-daemon`.

## API

[System.Nix.Store.Remote]: ./src/System/Nix/Store/Remote.hs

## Example

```haskell

import Data.HashSet as HS
import System.Nix.Store.Remote

main = do
  runStore $ do
    syncWithGC
    roots <- findRoots
    liftIO $ print roots

    res <- addTextToStore "hnix-store" "test" (HS.fromList [])  False
    print res
```
