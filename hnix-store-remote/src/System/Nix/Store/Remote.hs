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
  ) where

import           Control.Monad

import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

type RepairFlag = Bool
type CheckFlag = Bool

syncWithGC :: MonadStore ()
syncWithGC = runOp_ SyncWithGC

optimiseStore :: MonadStore ()
optimiseStore = runOp_ OptimiseStore

verifyStore :: CheckFlag -> RepairFlag -> MonadStore ()
verifyStore check repair = runOpArgs_ VerifyStore $ do
  putBool check
  putBool repair
