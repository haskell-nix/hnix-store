{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module System.Nix.Store.Remote (
    runStore
  , addToStore
  , syncWithGC
  , optimiseStore
  , verifyStore
  ) where

import           Prelude                 as P
import           Control.Monad
import           Control.Monad.Except

import           Data.Text               as T
import           Data.Text.Encoding      as T
import           Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding as TL

import           System.Nix.Hash
import           System.Nix.Internal.Hash
import           System.Nix.Nar
import           System.Nix.StorePath
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util
import           System.Nix.Util

import           Data.ByteString.Lazy as LBS
import           Data.ByteString.Char8 as BS


type RepairFlag = Bool
type CheckFlag = Bool
type RecursiveFlag = Bool

syncWithGC :: MonadStore s ()
syncWithGC = void $ simpleOp SyncWithGC

optimiseStore :: MonadStore s ()
optimiseStore = void $ simpleOp OptimiseStore

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore s Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair

addToStore :: forall hashType storeDir. (NamedAlgo hashType, ValidAlgo hashType, KnownStoreDir storeDir)
    => StorePathName
    -> FilePath
    -> RecursiveFlag
    -> FilePathFilter
    -> RepairFlag
    -> MonadStore storeDir (StorePath storeDir)
addToStore name srcPath recursive pathFilter repair = do
  when repair $ throwError "addToStore: Cannot repair when using a daemon."
  nar <- liftIO $ localPackNar' narEffectsIO srcPath pathFilter
  runOpArgs AddToStore $ do
    putByteStringLen $ strToN $ unStorePathName name
    putBool $ not (recursive && algoName @hashType == "sha256") -- backward compatibility hack
    putBool recursive
    putByteStringLen $ strToN (algoName @hashType)
    putNar nar

  path <- LBS.toStrict <$> sockGetStr
  case makeStorePath path of
    Just storePath -> return storePath
    Nothing -> throwError $ "Path '" ++ (show path) ++ "' is not a valid store path in this store"

 where
  strToN = TL.encodeUtf8 . TL.fromStrict
  
  makeStorePath :: BS.ByteString -> Maybe (StorePath storeDir)
  makeStorePath path = BS.stripPrefix (storeDirVal @storeDir <> "/") path >>= \basename ->
    if '/' `BS.elem` basename
    then Nothing
    else let (drvHash, drvName) = BS.break (== '-') basename in
      if BS.length drvHash /= 32 || BS.length drvName <= 1
      then Nothing
      else StorePath (Digest drvHash) <$> (makeStorePathName $ T.tail $ T.decodeUtf8 $ drvName)




