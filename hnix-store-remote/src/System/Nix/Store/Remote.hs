{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module System.Nix.Store.Remote (
    runStore
  , addToStore
  , syncWithGC
  , optimiseStore
  , verifyStore
  ) where

import           Control.Monad
import           Control.Monad.Except

import           Data.Text.Lazy       as TL
import           Data.Text.Lazy.Encoding as TL

import           System.Nix.Hash
import           System.Nix.Nar
import           System.Nix.StorePath
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util
import           System.Nix.Util

import           Data.ByteString.Lazy as LBS


type RepairFlag = Bool
type CheckFlag = Bool
type RecursiveFlag = Bool
type PathFilter = FilePath -> Bool

syncWithGC :: MonadStore ()
syncWithGC = void $ simpleOp SyncWithGC

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair

addToStore :: forall hashAlgo. (NamedAlgo hashAlgo, ValidAlgo hashAlgo)
    => StorePathName
    -> FilePath
    -> RecursiveFlag
    -> PathFilter
    -> RepairFlag
    -> MonadStore LBS.ByteString
addToStore name srcPath recursive filter repair = do
  nar <- liftIO $ localPackNar narEffectsIO srcPath -- TODO actually use filter.
  runOpArgs AddToStore $ do
    putByteStringLen $ strToN $ unStorePathName name
    putBool $ not (recursive && algoName @hashAlgo == "sha256") -- backwards compatibility hack
    putBool recursive
    putByteStringLen $ strToN (algoName @hashAlgo)
    putNar nar

  sockGetStr
 where
  strToN = TL.encodeUtf8 . TL.fromStrict
