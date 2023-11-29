{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings   #-}

module System.Nix.Store.Remote
  ( addToStore
  , addTextToStore
  , addSignatures
  , addIndirectRoot
  , addTempRoot
  , buildPaths
  , buildDerivation
  , deleteSpecific
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
  , module System.Nix.Store.Types
  , module System.Nix.Store.Remote.Types
  ) where

import Data.Dependent.Sum (DSum((:=>)))
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word
import qualified Data.Text
import qualified Control.Monad
import qualified Data.Attoparsec.Text
import qualified Data.Text.Encoding
import qualified System.Nix.Hash
--
import qualified Data.ByteString.Lazy          as BSL

import System.Nix.Derivation (Derivation)
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))
import           System.Nix.Build               ( BuildMode
                                                , BuildResult
                                                )
import           System.Nix.Hash                ( NamedAlgo(..)
                                                , BaseEncoding(Base16)
                                                , decodeDigestWith
                                                )
import           System.Nix.StorePath           ( StorePath
                                                , StorePathName
                                                , StorePathHashPart
                                                , InvalidPathError
                                                )
import           System.Nix.StorePath.Metadata  ( Metadata(..)
                                                , StorePathTrust(..)
                                                )

import qualified Data.Binary.Put
import qualified Data.Map.Strict
import qualified Data.Set

import qualified System.Nix.ContentAddress
import qualified System.Nix.StorePath

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util
import qualified System.Nix.Signature
import           Crypto.Hash                    ( SHA256 )
import           System.Nix.Nar                 ( NarSource )


-- | Pack `Nar` and add it to the store.
addToStore
  :: forall a
   . (NamedAlgo a)
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource MonadStore -- ^ provide nar stream
  -> FileIngestionMethod  -- ^ Add target directory recursively
  -> RepairMode           -- ^ Only used by local store backend
  -> MonadStore StorePath
addToStore name source recursive repair = do
  Control.Monad.when (repair == RepairMode_DoRepair)
    $ error "repairing is not supported when building through the Nix daemon"

  runOpArgsIO AddToStore $ \yield -> do
    yield $ BSL.toStrict $ Data.Binary.Put.runPut $ do
      putText $ System.Nix.StorePath.unStorePathName name
      putBool
        $ not
        $ System.Nix.Hash.algoName @a == "sha256"
          && recursive == FileIngestionMethod_FileRecursive
      putBool (recursive == FileIngestionMethod_FileRecursive)
      putText $ System.Nix.Hash.algoName @a
    source yield
  sockGetPath

-- | Add text to store.
--
-- Reference accepts repair but only uses it
-- to throw error in case of remote talking to nix-daemon.
addTextToStore
  :: Text              -- ^ Name of the text
  -> Text              -- ^ Actual text to add
  -> HashSet StorePath -- ^ Set of `StorePath`s that the added text references
  -> RepairMode        -- ^ Repair mode, must be `RepairMode_DontRepair` for remote backend
                       --   (only valid for local store)
  -> MonadStore StorePath
addTextToStore name text references' repair = do
  Control.Monad.when (repair == RepairMode_DoRepair)
    $ error "repairing is not supported when building through the Nix daemon"

  storeDir <- getStoreDir
  runOpArgs AddTextToStore $ do
    putText name
    putText text
    putPaths storeDir references'
  sockGetPath

addSignatures :: StorePath -> [BSL.ByteString] -> MonadStore ()
addSignatures p signatures = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs AddSignatures $ do
    putPath storeDir p
    putByteStrings signatures

addIndirectRoot :: StorePath -> MonadStore ()
addIndirectRoot pn = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs AddIndirectRoot $ putPath storeDir pn

-- | Add temporary garbage collector root.
--
-- This root is removed as soon as the client exits.
addTempRoot :: StorePath -> MonadStore ()
addTempRoot pn = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs AddTempRoot $ putPath storeDir pn

-- | Build paths if they are an actual derivations.
--
-- If derivation output paths are already valid, do nothing.
buildPaths :: HashSet StorePath -> BuildMode -> MonadStore ()
buildPaths ps bm = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs BuildPaths $ do
    putPaths storeDir ps
    putInt $ fromEnum bm

buildDerivation
  :: StorePath
  -> Derivation StorePath Text
  -> BuildMode
  -> MonadStore BuildResult
buildDerivation p drv buildMode = do
  storeDir <- getStoreDir
  runOpArgs BuildDerivation $ do
    putPath storeDir p
    putDerivation storeDir drv
    putEnum buildMode
    -- XXX: reason for this is unknown
    -- but without it protocol just hangs waiting for
    -- more data. Needs investigation.
    -- Intentionally the only warning that should pop-up.
    putInt (0 :: Integer)

  getSocketIncremental getBuildResult

-- | Delete store paths
deleteSpecific
 :: HashSet StorePath -- ^ Paths to delete
 -> MonadStore (HashSet StorePath, Word64) -- ^ (Paths deleted, Bytes freed)
deleteSpecific paths = do
  storeDir <- getStoreDir
  runOpArgs CollectGarbage $ do
    putEnum GCDeleteSpecific
    putPaths storeDir paths
    putBool False -- ignoreLiveness
    putInt (maxBound :: Word64) -- maxFreedBytes
    putInt (0::Int)
    putInt (0::Int)
    putInt (0::Int)
  getSocketIncremental $ do
    deletedPaths <- getPaths storeDir
    bytesFreed <- getInt
    _ :: Int <- getInt
    pure (deletedPaths, bytesFreed)

ensurePath :: StorePath -> MonadStore ()
ensurePath pn = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs EnsurePath $ putPath storeDir pn

-- | Find garbage collector roots.
findRoots :: MonadStore (Map BSL.ByteString StorePath)
findRoots = do
  runOp FindRoots
  sd  <- getStoreDir
  res <-
    getSocketIncremental
    $ getMany
    $ (,)
      <$> (BSL.fromStrict <$> getByteStringLen)
      <*> getPath sd

  r <- catRights res
  pure $ Data.Map.Strict.fromList r
 where
  catRights :: [(a, Either InvalidPathError b)] -> MonadStore [(a, b)]
  catRights = mapM ex

  ex :: (a, Either InvalidPathError b) -> MonadStore (a, b)
  ex (x , Right y) = pure (x, y)
  ex (_x, Left e ) = error $ "Unable to decode root: " <> show e

isValidPathUncached :: StorePath -> MonadStore Bool
isValidPathUncached p = do
  storeDir <- getStoreDir
  simpleOpArgs IsValidPath $ putPath storeDir p

-- | Query valid paths from set, optionally try to use substitutes.
queryValidPaths
  :: HashSet StorePath   -- ^ Set of `StorePath`s to query
  -> SubstituteFlag -- ^ Try substituting missing paths when `True`
  -> MonadStore (HashSet StorePath)
queryValidPaths ps substitute = do
  storeDir <- getStoreDir
  runOpArgs QueryValidPaths $ do
    putPaths storeDir ps
    putBool (unSubstituteFlag substitute)
  sockGetPaths

queryAllValidPaths :: MonadStore (HashSet StorePath)
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: HashSet StorePath -> MonadStore (HashSet StorePath)
querySubstitutablePaths ps = do
  storeDir <- getStoreDir
  runOpArgs QuerySubstitutablePaths $ putPaths storeDir ps
  sockGetPaths

queryPathInfoUncached :: StorePath -> MonadStore (Metadata StorePath)
queryPathInfoUncached path = do
  storeDir <- getStoreDir
  runOpArgs QueryPathInfo $ do
    putPath storeDir path

  valid <- sockGetBool
  Control.Monad.unless valid $ error "Path is not valid"

  deriverPath <- sockGetPathMay

  narHashText <- Data.Text.Encoding.decodeUtf8 <$> sockGetStr
  let
    narHash =
      case
        decodeDigestWith @SHA256 Base16 narHashText
        of
        Left  e -> error e
        Right d -> System.Nix.Hash.HashAlgo_SHA256 :=> d

  references       <- sockGetPaths
  registrationTime <- sockGet getTime
  narBytes         <- Just <$> sockGetInt
  ultimate         <- sockGetBool

  sigStrings       <- fmap bsToText <$> sockGetStrings
  caString         <- bsToText <$> sockGetStr

  let
      sigs = case
               Data.Set.fromList <$> mapM (Data.Attoparsec.Text.parseOnly System.Nix.Signature.signatureParser) sigStrings
               of
               Left e -> error e
               Right x -> x

      contentAddress =
        if Data.Text.null caString then Nothing else
        case
          Data.Attoparsec.Text.parseOnly
            System.Nix.ContentAddress.contentAddressParser
            caString
          of
          Left  e -> error e
          Right x -> Just x

      trust = if ultimate then BuiltLocally else BuiltElsewhere

  pure $ Metadata{..}

queryReferrers :: StorePath -> MonadStore (HashSet StorePath)
queryReferrers p = do
  storeDir <- getStoreDir
  runOpArgs QueryReferrers $ putPath storeDir p
  sockGetPaths

queryValidDerivers :: StorePath -> MonadStore (HashSet StorePath)
queryValidDerivers p = do
  storeDir <- getStoreDir
  runOpArgs QueryValidDerivers $ putPath storeDir p
  sockGetPaths

queryDerivationOutputs :: StorePath -> MonadStore (HashSet StorePath)
queryDerivationOutputs p = do
  storeDir <- getStoreDir
  runOpArgs QueryDerivationOutputs $ putPath storeDir p
  sockGetPaths

queryDerivationOutputNames :: StorePath -> MonadStore (HashSet StorePath)
queryDerivationOutputNames p = do
  storeDir <- getStoreDir
  runOpArgs QueryDerivationOutputNames $ putPath storeDir p
  sockGetPaths

queryPathFromHashPart :: StorePathHashPart -> MonadStore StorePath
queryPathFromHashPart storePathHash = do
  runOpArgs QueryPathFromHashPart
    $ putText
    $ System.Nix.StorePath.storePathHashPartToText storePathHash
  sockGetPath

queryMissing
  :: (HashSet StorePath)
  -> MonadStore
      ( HashSet StorePath -- Paths that will be built
      , HashSet StorePath -- Paths that have substitutes
      , HashSet StorePath -- Unknown paths
      , Integer           -- Download size
      , Integer           -- Nar size?
      )
queryMissing ps = do
  storeDir <- getStoreDir
  runOpArgs QueryMissing $ putPaths storeDir ps

  willBuild      <- sockGetPaths
  willSubstitute <- sockGetPaths
  unknown        <- sockGetPaths
  downloadSize'  <- sockGetInt
  narSize'       <- sockGetInt
  pure (willBuild, willSubstitute, unknown, downloadSize', narSize')

optimiseStore :: MonadStore ()
optimiseStore = Control.Monad.void $ simpleOp OptimiseStore

syncWithGC :: MonadStore ()
syncWithGC = Control.Monad.void $ simpleOp SyncWithGC

-- returns True on errors
verifyStore :: CheckFlag -> RepairMode -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool $ unCheckFlag check
  putBool $ repair == RepairMode_DoRepair
