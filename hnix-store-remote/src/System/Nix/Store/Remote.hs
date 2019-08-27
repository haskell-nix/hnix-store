{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module System.Nix.Store.Remote (
    BuildMode(..)
  , runStore
  , syncWithGC
  , optimiseStore
  , verifyStore
  , buildPaths
  ) where

import           Data.Binary.Put (Put, putInthost)
import           Data.ByteString (ByteString)
import           System.Nix.Util
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

data BuildMode = Normal | Repair | Check
  deriving (Eq, Show)

putBuildMode :: BuildMode -> Put
putBuildMode mode = putInthost $
  case mode of
    Normal -> 0
    Repair -> 1
    Check -> 2

buildPaths ::
  -- forall storeDir . (KnownStoreDir storeDir) =>
  -- [StorePath storeDir]
  [ByteString] -> BuildMode -> MonadStore ()
buildPaths drvs mode =
  runOpArgs_ BuildPaths args
  where
    args = do
      putByteStrings drvs
      putBuildMode mode
