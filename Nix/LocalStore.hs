{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{- |
Module      : Nix.LocalStore
Description : Local implementation of the nix store API
Copyright   : Copyright (c) 2014 Shea Levy
License     : MIT
Maintainer  : shea@shealevy.com
Stability   : Experimental
Portability : Portable

The @LocalStore@ module contains an implementation of the nix store API that
interacts directly with the nix store and nix database.
-}
module Nix.LocalStore
    ( -- * The @LocalStore@ type
      LocalStore
    , allocateLocalStore
    ) where

import Control.Monad.Trans.Resource
    ( MonadResource
    , ReleaseKey
    , allocate
    )
import qualified Database.SQLite.Simple as DB

import Nix.Store (Store, isValidPath)

-- | Instance of the 'Store' class that directly runs builders and modifies
-- the database
data LocalStore = LocalStore
    { connection :: DB.Connection  -- ^ The database connection
    }

-- | Allocate a @LocalStore@ within a @MonadResource@.
allocateLocalStore :: MonadResource m
                   => FilePath  -- ^ Path to the database
                   -> m (ReleaseKey, LocalStore)
allocateLocalStore path = allocate alloc dealloc
  where
    alloc = DB.open path >>= return . LocalStore
    dealloc (LocalStore c) = DB.close c

instance Store LocalStore where
    isValidPath (LocalStore c) path = do
        r <- DB.query c "IF EXISTS (SELECT 1 FROM validpaths WHERE path=(?)) SELECT 1 ELSE SELECT 0" (DB.Only path)
        case (head r) of
            (DB.Only i) -> return i
