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

An implementation of the nix store API that interacts directly with the nix
store and nix database.
-}
module Nix.LocalStore
    ( -- * The @LocalStore@ type
      LocalStore
    , allocateLocalStore
    ) where

import qualified Data.Text as T
import qualified Data.HashSet as HS
import Control.Monad.Trans.Resource
    ( MonadResource
    , ReleaseKey
    , allocate
    )
import qualified Database.SQLite.Simple as DB

import Nix.Types (PathSet)
import qualified Nix.Store as S

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

-- | Helper for the common case of queries returning @PathSet@
extractPathSet :: [[FilePath]] -> IO PathSet
extractPathSet = return . HS.fromList . concat

instance S.Store LocalStore where
    isValidPath (LocalStore c) path = do
        DB.query c "SELECT EXISTS (SELECT 1 FROM validpaths WHERE path=(?) LIMIT 1)" (DB.Only path)
            >>= return . DB.fromOnly . head

    queryValidPaths (LocalStore c) paths
        | HS.null paths = return paths
        | otherwise     =
            DB.query c (DB.Query qfinal) (HS.toList paths) >>= extractPathSet
      where
        qbase = "SELECT path FROM validpaths WHERE path IN ("
        qvariable = T.intersperse ',' . flip T.replicate "?" $ HS.size paths
        qfinal = T.append qbase $ T.snoc qvariable ')'

    queryAllValidPaths (LocalStore c) =
        DB.query_ c "SELECT path FROM validpaths" >>= extractPathSet
