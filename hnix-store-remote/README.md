# hnix-store-remote

[Nix] worker protocol implementation for interacting with remote Nix store
via `nix-daemon`.

[Nix]: https://nixos.org/nix

## API

[System.Nix.Store.Remote]: ./src/System/Nix/Store/Remote.hs

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import System.Nix.StorePath (mkStorePathName)
import System.Nix.Store.Remote

main :: IO ()
main = do
  void $ runStore $ do
    syncWithGC
    roots <- findRoots
    liftIO $ print roots

    res <- case mkStorePathName "hnix-store" of
      Left e -> error (show e)
      Right name ->
        addTextToStore
         (StoreText name "Hello World!")
         mempty
         RepairMode_DontRepair
    liftIO $ print res
```
