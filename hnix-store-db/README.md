# hnix-store-db

[Nix] SQLite database implementation.

Only read-only functionality provided for
database schema version `10`.

[Nix]: https://nixos.org/nix

## API

The interface is experimental and might change wildly.

[System.Nix.Store.DB.Query]: ./src/System/Nix/Store/DB/Query.hs
[System.Nix.Store.DB.Run]: ./src/System/Nix/Store/DB/Run.hs
[System.Nix.Store.DB.Schema]: ./src/System/Nix/Store/DB/Schema.hs

## Example

This example is runnable via `cabal run db-readme`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Default.Class (Default(def))

import Control.Monad qualified
import Control.Monad.IO.Class qualified

import Database.Esqueleto.Experimental qualified

import System.Nix.StorePath qualified
import System.Nix.Store.DB.Run qualified
import System.Nix.Store.DB.Schema qualified

import System.Nix.Store.DB.Query

main :: IO ()
main = do
  System.Nix.Store.DB.Run.runSystemSqlite $ do
    (paths, refs, drvOuts) <- queryEverything

    Control.Monad.IO.Class.liftIO $ do
      putStrLn $ "Stats: "
      let stat name v = putStrLn $ "- " ++ name ++ ": " ++ show (length v)
      stat "ValidPath(s)" paths
      stat "Ref(s)" refs
      stat "DerivationOutput(s)" drvOuts

    maybeValidPath <- queryOneValidDerivationEntity
    case maybeValidPath of
      Nothing -> pure ()
      Just validPathEntity -> do
        let pth =
              System.Nix.Store.DB.Schema.validPathPath
              $ Database.Esqueleto.Experimental.entityVal validPathEntity

        (same, samePath, references, referrers, validDerivers, outputs) <- (,,,,,)
          <$> queryPathInfo pth
          <*> queryPathFromHashPart def (System.Nix.StorePath.storePathHash pth)
          <*> queryReferences validPathEntity
          <*> queryReferrers pth
          <*> queryValidDerivers pth
          <*> queryDerivationOutputs validPathEntity

        Control.Monad.unless (same == Just (Database.Esqueleto.Experimental.entityVal validPathEntity))
          $ error "queryPathInfo failed to roundtrip"
        Control.Monad.unless (samePath == Just pth)
          $ error "queryPathFromHashPart failed to roundtrip"

        Control.Monad.IO.Class.liftIO $ do
          putStrLn $ "References: "
          print references
          putStrLn $ "Referrers: "
          print referrers
          putStrLn $ "Valid derivers: "
          print validDerivers
          putStrLn $ "Derivation outputs: "
          print outputs

    pure ()
```
