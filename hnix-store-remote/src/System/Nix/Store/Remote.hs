{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module System.Nix.Store.Remote (
    runStore
  , syncWithGC
  , optimiseStore
  , verifyStore
  , computeFSClosure
  ) where

import           Control.Monad
import           Data.Foldable

import           System.Nix.Internal.StorePath
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

type RepairFlag = Bool
type CheckFlag = Bool

syncWithGC :: MonadStore ()
syncWithGC = void $ simpleOp SyncWithGC

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair

computeClosure
  :: (Monad m, Foldable f, Monoid a)
  => (a -> m (f r))
  -> (r -> a)
  -> a
  -> m a
computeClosure lookupReferences findReferences = go
  where
    go parents = do
      rs <- lookupReferences parents
      (parents <>) <$> children rs
    children rs =
      if null rs
      then pure mempty
      else go (foldMap findReferences rs)

computeFSClosure
  :: StorePathSet storeDir
  -> MonadStore (StorePathSet storeDir)
computeFSClosure =
  computeClosure (traverse queryPathInfoUncached . toList) referencesVP
