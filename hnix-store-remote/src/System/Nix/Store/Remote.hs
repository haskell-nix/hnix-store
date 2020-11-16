{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
module System.Nix.Store.Remote
  (
    RemoteStoreT
  , System.Nix.Nar.PathType (..)
  , addToStore
  , addTextToStore
  , addSignatures
  , addIndirectRoot
  , addTempRoot
  , buildPaths
  , buildDerivation
  , ensurePath
  , findRoots
  , isValidPathUncached
  , queryValidPaths
  , queryAllValidPaths
  , querySubstitutablePaths
  , queryPathInfoUncached
  , queryReferrers
  , queryValidDerivers
  , queryDerivationOutputs
  , queryDerivationOutputNames
  , queryPathFromHashPart
  , queryMissing
  , optimiseStore
  , runStore
  , syncWithGC
  , verifyStore
  )
  where

import Control.Monad (void, unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)

import Nix.Derivation (Derivation)
import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Hash (Digest, NamedAlgo, ValidAlgo, SomeNamedDigest(..))
import System.Nix.StorePath (StorePath, StorePathName, StorePathSet, StorePathHashAlgo)
import System.Nix.StorePathMetadata (StorePathMetadata(..), StorePathTrust(..))

import qualified Control.Monad.IO.Class
import qualified Data.Binary.Put
import qualified Data.ByteString.Lazy
import qualified Data.Map.Strict
import qualified Data.Set
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy

import qualified System.Nix.Nar
import qualified System.Nix.Hash
import qualified System.Nix.StorePath
import qualified System.Nix.Store.Remote.Builders
import qualified System.Nix.Store.Remote.Parsers

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

type RepairFlag = Bool
type CheckFlag = Bool
type CheckSigsFlag = Bool
type SubstituteFlag = Bool

-- | Pack `FilePath` as `Nar` and add it to the store.
addToStore :: forall a m. (NamedAlgo a, MonadRemoteStore m, MonadIO m)
           => StorePathName        -- ^ Name part of the newly created `StorePath`
           -> FilePath             -- ^ Local `FilePath` to add
           -> Bool                 -- ^ Add target directory recursively
           -> (FilePath -> System.Nix.Nar.PathType -> m Bool)   -- ^ Path filter function
           -> RepairFlag           -- ^ Only used by local store backend
           -> m StorePath
addToStore name pth recursive pathFilter _repair = do

  runOpArgsIO AddToStore $ \yield -> do
    yield $ Data.ByteString.Lazy.toStrict $ Data.Binary.Put.runPut $ do
      putText $ System.Nix.StorePath.unStorePathName name

      putBool
        $ not
        $ System.Nix.Hash.algoName @a == "sha256" && recursive

      putBool recursive

      putText $ System.Nix.Hash.algoName @a

    System.Nix.Nar.streamNarIO yield pathFilter System.Nix.Nar.narEffectsIO pth

  sockGetPath

-- | Add text to store.
--
-- Reference accepts repair but only uses it
-- to throw error in case of remote talking to nix-daemon.
addTextToStore :: (MonadIO m, MonadRemoteStore m)
               => Text         -- ^ Name of the text
               -> Text         -- ^ Actual text to add
               -> StorePathSet -- ^ Set of `StorePath`s that the added text references
               -> RepairFlag   -- ^ Repair flag, must be `False` in case of remote backend
               -> m StorePath
addTextToStore name text references' repair = do
  when repair $ error "repairing is not supported when building through the Nix daemon"
  runOpArgs AddTextToStore $ do
    putText name
    putText text
    putPaths references'
  sockGetPath

addSignatures :: (MonadIO m)
              => StorePath
              -> [ByteString]
              -> RemoteStoreT m ()
addSignatures p signatures = do
  void $ simpleOpArgs AddSignatures $ do
    putPath p
    putByteStrings signatures

addIndirectRoot :: (MonadIO m) => StorePath -> RemoteStoreT m ()
addIndirectRoot pn = do
  void $ simpleOpArgs AddIndirectRoot $ putPath pn

-- | Add temporary garbage collector root.
--
-- This root is removed as soon as the client exits.
addTempRoot :: (MonadIO m) => StorePath -> RemoteStoreT m ()
addTempRoot pn = do
  void $ simpleOpArgs AddTempRoot $ putPath pn

-- | Build paths if they are an actual derivations.
--
-- If derivation output paths are already valid, do nothing.
buildPaths :: (MonadIO m)
           => StorePathSet
           -> BuildMode
           -> RemoteStoreT m ()
buildPaths ps bm = do
  void $ simpleOpArgs BuildPaths $ do
    putPaths ps
    putInt $ fromEnum bm

buildDerivation :: (MonadIO m)
                => StorePath
                -> Derivation StorePath Text
                -> BuildMode
                -> RemoteStoreT m BuildResult
buildDerivation p drv buildMode = do
  runOpArgs BuildDerivation $ do
    putPath p
    putDerivation drv
    putEnum buildMode
    -- XXX: reason for this is unknown
    -- but without it protocol just hangs waiting for
    -- more data. Needs investigation
    putInt (0 :: Int)

  res <- getSocketIncremental $ getBuildResult
  return res

ensurePath :: (MonadIO m) => StorePath -> RemoteStoreT m ()
ensurePath pn = do
  void $ simpleOpArgs EnsurePath $ putPath pn

-- | Find garbage collector roots.
findRoots :: (MonadIO m) => RemoteStoreT m (Map ByteString StorePath)
findRoots = do
  runOp FindRoots
  sd <- getStoreDir
  res <- getSocketIncremental
    $ getMany
    $ (,) <$> (Data.ByteString.Lazy.fromStrict <$> getByteStringLen)
          <*> getPath sd

  r <- catRights res
  return $ Data.Map.Strict.fromList r
  where
    catRights :: (MonadIO m) => [(a, Either String b)] -> RemoteStoreT m [(a, b)]
    catRights = mapM ex

    ex :: (MonadIO m) => (a, Either [Char] b) -> RemoteStoreT m (a, b)
    ex (x, Right y) = return (x, y)
    ex (_x , Left e) = error $ "Unable to decode root: "  ++ e

isValidPathUncached :: (MonadIO m) => StorePath -> RemoteStoreT m Bool
isValidPathUncached p = do
  simpleOpArgs IsValidPath $ putPath p

-- | Query valid paths from set, optionally try to use substitutes.
queryValidPaths :: (MonadIO m)
                => StorePathSet   -- ^ Set of `StorePath`s to query
                -> SubstituteFlag -- ^ Try substituting missing paths when `True`
                -> RemoteStoreT m StorePathSet
queryValidPaths ps substitute = do
  runOpArgs QueryValidPaths $ do
    putPaths ps
    putBool substitute
  sockGetPaths

queryAllValidPaths :: (MonadIO m) => RemoteStoreT m StorePathSet
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: (MonadIO m) => StorePathSet -> RemoteStoreT m StorePathSet
querySubstitutablePaths ps = do
  runOpArgs QuerySubstitutablePaths $ do
    putPaths ps
  sockGetPaths

queryPathInfoUncached :: (MonadIO m)
                      => StorePath
                      -> RemoteStoreT m StorePathMetadata
queryPathInfoUncached path = do
  runOpArgs QueryPathInfo $ do
    putPath path

  valid <- sockGetBool
  unless valid $ error "Path is not valid"

  deriverPath <- sockGetPathMay

  narHashText <- Data.Text.Encoding.decodeUtf8 <$> sockGetStr
  let narHash = case System.Nix.Hash.decodeBase32 @'System.Nix.Hash.SHA256 narHashText of
        Left e -> error e
        Right x -> SomeDigest x

  references <- sockGetPaths
  registrationTime <- sockGet getTime
  narBytes <- Just <$> sockGetInt
  ultimate <- sockGetBool

  _sigStrings <- map bsToText <$> sockGetStrings
  caString <- sockGetStr

  let
      -- XXX: signatures need pubkey from config
      sigs = Data.Set.empty

      contentAddressableAddress =
        case System.Nix.Store.Remote.Parsers.parseContentAddressableAddress caString of
          Left e -> error e
          Right x -> Just x

      trust = if ultimate then BuiltLocally
                          else BuiltElsewhere

  return $ StorePathMetadata {..}

queryReferrers :: (MonadIO m) => StorePath -> RemoteStoreT m StorePathSet
queryReferrers p = do
  runOpArgs QueryReferrers $ do
    putPath p
  sockGetPaths

queryValidDerivers :: (MonadIO m) => StorePath -> RemoteStoreT m StorePathSet
queryValidDerivers p = do
  runOpArgs QueryValidDerivers $ do
    putPath p
  sockGetPaths

queryDerivationOutputs :: (MonadIO m) => StorePath -> RemoteStoreT m StorePathSet
queryDerivationOutputs p = do
  runOpArgs QueryDerivationOutputs $
    putPath p
  sockGetPaths

queryDerivationOutputNames :: (MonadIO m) => StorePath -> RemoteStoreT m StorePathSet
queryDerivationOutputNames p = do
  runOpArgs QueryDerivationOutputNames $
    putPath p
  sockGetPaths

queryPathFromHashPart :: (MonadIO m) => Digest StorePathHashAlgo -> RemoteStoreT m StorePath
queryPathFromHashPart storePathHash = do
  runOpArgs QueryPathFromHashPart $
    putByteStringLen
      $ Data.ByteString.Lazy.fromStrict
      $ Data.Text.Encoding.encodeUtf8
      $ System.Nix.Hash.encodeBase32 storePathHash
  sockGetPath

queryMissing :: (MonadIO m)
             => StorePathSet
             -> RemoteStoreT m ( StorePathSet -- Paths that will be built
                              , StorePathSet -- Paths that have substitutes
                              , StorePathSet -- Unknown paths
                              , Integer      -- Download size
                              , Integer)     -- Nar size?
queryMissing ps = do
  runOpArgs QueryMissing $ do
    putPaths ps

  willBuild      <- sockGetPaths
  willSubstitute <- sockGetPaths
  unknown        <- sockGetPaths
  downloadSize'  <- sockGetInt
  narSize'       <- sockGetInt
  return (willBuild, willSubstitute, unknown, downloadSize', narSize')

optimiseStore :: (MonadIO m) => RemoteStoreT m ()
optimiseStore = void $ simpleOp OptimiseStore

syncWithGC :: (MonadIO m) => RemoteStoreT m ()
syncWithGC = void $ simpleOp SyncWithGC

-- returns True on errors
verifyStore :: (MonadIO m) => CheckFlag -> RepairFlag -> RemoteStoreT m Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair
