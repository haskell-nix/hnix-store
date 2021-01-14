{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
module System.Nix.Store.Remote
  (
    addToStore
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
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)

import Nix.Derivation (Derivation)
import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Hash (Digest, NamedAlgo, ValidAlgo, SomeNamedDigest(..), BaseEncoding(Base32))
import System.Nix.StorePath (StorePath, StorePathName, StorePathSet, StorePathHashAlgo)
import System.Nix.StorePathMetadata (StorePathMetadata(..), StorePathTrust(..))

import qualified Data.Binary.Put
import qualified Data.ByteString.Lazy
import qualified Data.Map.Strict
import qualified Data.Set
import qualified Data.Text.Encoding

import qualified System.Nix.Nar
import qualified System.Nix.Hash
import qualified System.Nix.StorePath
import qualified System.Nix.Store.Remote.Parsers

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

type RepairFlag = Bool
type CheckFlag = Bool
type SubstituteFlag = Bool

-- | Pack `FilePath` as `Nar` and add it to the store.
addToStore :: forall a. (ValidAlgo a, NamedAlgo a)
           => StorePathName        -- ^ Name part of the newly created `StorePath`
           -> FilePath             -- ^ Local `FilePath` to add
           -> Bool                 -- ^ Add target directory recursively
           -> (FilePath -> Bool)   -- ^ Path filter function
           -> RepairFlag           -- ^ Only used by local store backend
           -> MonadStore StorePath
addToStore name pth recursive _pathFilter _repair = do

  runOpArgsIO AddToStore $ \yield -> do
    yield $ Data.ByteString.Lazy.toStrict $ Data.Binary.Put.runPut $ do
      putText $ System.Nix.StorePath.unStorePathName name

      putBool
        $ not
        $ System.Nix.Hash.algoName @a == "sha256" && recursive

      putBool recursive

      putText $ System.Nix.Hash.algoName @a

    System.Nix.Nar.streamNarIO yield System.Nix.Nar.narEffectsIO pth

  sockGetPath

-- | Add text to store.
--
-- Reference accepts repair but only uses it
-- to throw error in case of remote talking to nix-daemon.
addTextToStore :: Text         -- ^ Name of the text
               -> Text         -- ^ Actual text to add
               -> StorePathSet -- ^ Set of `StorePath`s that the added text references
               -> RepairFlag   -- ^ Repair flag, must be `False` in case of remote backend
               -> MonadStore StorePath
addTextToStore name text references' repair = do
  when repair $ error "repairing is not supported when building through the Nix daemon"
  runOpArgs AddTextToStore $ do
    putText name
    putText text
    putPaths references'
  sockGetPath

addSignatures :: StorePath
              -> [ByteString]
              -> MonadStore ()
addSignatures p signatures = do
  void $ simpleOpArgs AddSignatures $ do
    putPath p
    putByteStrings signatures

addIndirectRoot :: StorePath -> MonadStore ()
addIndirectRoot pn = do
  void $ simpleOpArgs AddIndirectRoot $ putPath pn

-- | Add temporary garbage collector root.
--
-- This root is removed as soon as the client exits.
addTempRoot :: StorePath -> MonadStore ()
addTempRoot pn = do
  void $ simpleOpArgs AddTempRoot $ putPath pn

-- | Build paths if they are an actual derivations.
--
-- If derivation output paths are already valid, do nothing.
buildPaths :: StorePathSet
           -> BuildMode
           -> MonadStore ()
buildPaths ps bm = do
  void $ simpleOpArgs BuildPaths $ do
    putPaths ps
    putInt $ fromEnum bm

buildDerivation :: StorePath
                -> Derivation StorePath Text
                -> BuildMode
                -> MonadStore BuildResult
buildDerivation p drv buildMode = do
  runOpArgs BuildDerivation $ do
    putPath p
    putDerivation drv
    putEnum buildMode
    -- XXX: reason for this is unknown
    -- but without it protocol just hangs waiting for
    -- more data. Needs investigation.
    -- Intentionally the only warning that should pop-up.
    putInt 0

  res <- getSocketIncremental $ getBuildResult
  return res

ensurePath :: StorePath -> MonadStore ()
ensurePath pn = do
  void $ simpleOpArgs EnsurePath $ putPath pn

-- | Find garbage collector roots.
findRoots :: MonadStore (Map ByteString StorePath)
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
    catRights :: [(a, Either String b)] -> MonadStore [(a, b)]
    catRights = mapM ex

    ex :: (a, Either [Char] b) -> MonadStore (a, b)
    ex (x, Right y) = return (x, y)
    ex (_x , Left e) = error $ "Unable to decode root: "  ++ e

isValidPathUncached :: StorePath -> MonadStore Bool
isValidPathUncached p = do
  simpleOpArgs IsValidPath $ putPath p

-- | Query valid paths from set, optionally try to use substitutes.
queryValidPaths :: StorePathSet   -- ^ Set of `StorePath`s to query
                -> SubstituteFlag -- ^ Try substituting missing paths when `True`
                -> MonadStore StorePathSet
queryValidPaths ps substitute = do
  runOpArgs QueryValidPaths $ do
    putPaths ps
    putBool substitute
  sockGetPaths

queryAllValidPaths :: MonadStore StorePathSet
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: StorePathSet -> MonadStore StorePathSet
querySubstitutablePaths ps = do
  runOpArgs QuerySubstitutablePaths $ do
    putPaths ps
  sockGetPaths

queryPathInfoUncached :: StorePath
                      -> MonadStore StorePathMetadata
queryPathInfoUncached path = do
  runOpArgs QueryPathInfo $ do
    putPath path

  valid <- sockGetBool
  unless valid $ error "Path is not valid"

  deriverPath <- sockGetPathMay

  narHashText <- Data.Text.Encoding.decodeUtf8 <$> sockGetStr
  let narHash = case System.Nix.Hash.decodeBase @'System.Nix.Hash.SHA256 Base32 narHashText of
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

queryReferrers :: StorePath -> MonadStore StorePathSet
queryReferrers p = do
  runOpArgs QueryReferrers $ do
    putPath p
  sockGetPaths

queryValidDerivers :: StorePath -> MonadStore StorePathSet
queryValidDerivers p = do
  runOpArgs QueryValidDerivers $ do
    putPath p
  sockGetPaths

queryDerivationOutputs :: StorePath -> MonadStore StorePathSet
queryDerivationOutputs p = do
  runOpArgs QueryDerivationOutputs $
    putPath p
  sockGetPaths

queryDerivationOutputNames :: StorePath -> MonadStore StorePathSet
queryDerivationOutputNames p = do
  runOpArgs QueryDerivationOutputNames $
    putPath p
  sockGetPaths

queryPathFromHashPart :: Digest StorePathHashAlgo -> MonadStore StorePath
queryPathFromHashPart storePathHash = do
  runOpArgs QueryPathFromHashPart $
    putByteStringLen
      $ Data.ByteString.Lazy.fromStrict
      $ Data.Text.Encoding.encodeUtf8
      $ System.Nix.Hash.encodeInBase Base32 storePathHash
  sockGetPath

queryMissing :: StorePathSet
             -> MonadStore ( StorePathSet -- Paths that will be built
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

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

syncWithGC :: MonadStore ()
syncWithGC = void $ simpleOp SyncWithGC

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair
