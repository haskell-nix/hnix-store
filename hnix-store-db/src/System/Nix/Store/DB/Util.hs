{-# LANGUAGE OverloadedStrings #-}
module System.Nix.Store.DB.Util
  ( persistLikeNix
  , setWAL
  , enableWAL
  , disableWAL
  , setFK
  , enableFK
  , disableFK
  ) where

import Language.Haskell.TH.Quote
import Database.Persist.Quasi
import Database.Persist.Sqlite (SqliteConnectionInfo)
import Database.Persist.TH (persistWith)

import qualified Database.Persist.Sqlite
import qualified Lens.Micro

-- | Coerce table names to their plural names
-- i.e. ValidPath -> ValidPaths
persistLikeNix :: QuasiQuoter
persistLikeNix = persistWith $
  setPsToDBName
    (coerce . (getPsToDBName upperCaseSettings))
    upperCaseSettings
  where
    coerce x | x `elem` ["ValidPath", "Ref", "DerivationOutput"] = plural x
    coerce x = x

    plural x = x <> "s"

-- * WAL and FK

-- | Configure WAL (write ahead log)
setWAL
  :: Bool
  -> SqliteConnectionInfo
  -> SqliteConnectionInfo
setWAL v = Lens.Micro.over Database.Persist.Sqlite.walEnabled (const v)

-- | Enable WAL (write ahead log)
enableWAL
  :: SqliteConnectionInfo
  -> SqliteConnectionInfo
enableWAL = setWAL True

-- | Disable WAL (write ahead log)
disableWAL
  :: SqliteConnectionInfo
  -> SqliteConnectionInfo
disableWAL = setWAL False

-- | Configure FK (foreign key constraints)
setFK
  :: Bool
  -> SqliteConnectionInfo
  -> SqliteConnectionInfo
setFK v = Lens.Micro.over Database.Persist.Sqlite.walEnabled (const v)

-- | Enable foreign key constraint checking
enableFK
  :: SqliteConnectionInfo
  -> SqliteConnectionInfo
enableFK = setFK True

-- | Disable foreign key constraint checking
disableFK
  :: SqliteConnectionInfo
  -> SqliteConnectionInfo
disableFK = setFK False
