{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings   #-}

module System.Nix.Store.Remote
  (
  -- * Operations
    addToStore
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
  , syncWithGC
  , verifyStore
  , module System.Nix.Store.Types
  , module System.Nix.Store.Remote.MonadStore
  , module System.Nix.Store.Remote.Types
  -- * Compat
  , MonadStore
  -- * Runners
  , runStore
  , runStoreOpts
  , runStoreOptsTCP
  ) where

import Crypto.Hash (SHA256)
import Data.ByteString (ByteString)
import Data.Default.Class (Default(def))
import Data.Dependent.Sum (DSum((:=>)))
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Network.Socket (Family, SockAddr(SockAddrUnix))
import System.Nix.Nar (NarSource)
import System.Nix.Derivation (Derivation)
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))
import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Hash (NamedAlgo(..), BaseEncoding(Base16), decodeDigestWith)
import System.Nix.StorePath (StoreDir(..), StorePath, StorePathName, StorePathHashPart, InvalidPathError)
import System.Nix.StorePath.Metadata  (Metadata(..), StorePathTrust(..))

import qualified Data.Text
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Attoparsec.Text
import qualified Data.Text.Encoding
import qualified Data.Map.Strict
import qualified Data.Serialize.Put
import qualified Data.Set
import qualified Network.Socket

import qualified System.Nix.ContentAddress
import qualified System.Nix.Hash
import qualified System.Nix.Signature
import qualified System.Nix.StorePath

import System.Nix.Store.Remote.MonadStore (RemoteStoreT, getStoreDir, RemoteStoreError(RemoteStoreError_GetAddrInfoFailed))
import System.Nix.Store.Remote.Client (Run, runStoreSocket, runOp, runOpArgs, runOpArgsIO, simpleOp, simpleOpArgs)
import System.Nix.Store.Remote.Socket
import System.Nix.Store.Remote.Types

import Data.Serialize (get)
import System.Nix.Store.Remote.Serialize (putDerivation)
import System.Nix.Store.Remote.Serialize.Prim

-- * Compat

type MonadStore = RemoteStoreT StoreConfig IO

-- * Runners

runStore :: MonadStore a -> Run IO a
runStore = runStoreOpts defaultSockPath def
  where
    defaultSockPath :: String
    defaultSockPath = "/nix/var/nix/daemon-socket/socket"

runStoreOpts
  :: FilePath
  -> StoreDir
  -> MonadStore a
  -> Run IO a
runStoreOpts socketPath =
  runStoreOpts'
    Network.Socket.AF_UNIX
    (SockAddrUnix socketPath)

runStoreOptsTCP
  :: String
  -> Int
  -> StoreDir
  -> MonadStore a
  -> Run IO a
runStoreOptsTCP host port sd code = do
  Network.Socket.getAddrInfo
    (Just Network.Socket.defaultHints)
    (Just host)
    (Just $ show port)
    >>= \case
      (sockAddr:_) ->
        runStoreOpts'
          (Network.Socket.addrFamily sockAddr)
          (Network.Socket.addrAddress sockAddr)
          sd
          code
      _ -> pure (Left RemoteStoreError_GetAddrInfoFailed, [])

runStoreOpts'
  :: Family
  -> SockAddr
  -> StoreDir
  -> MonadStore a
  -> Run IO a
runStoreOpts' sockFamily sockAddr storeRootDir code =
  Control.Exception.bracket
    open
    (Network.Socket.close . hasStoreSocket)
    (flip runStoreSocket code)
  where
    open = do
      soc <- Network.Socket.socket sockFamily Network.Socket.Stream 0
      Network.Socket.connect soc sockAddr
      pure PreStoreConfig
          { preStoreConfig_socket = soc
          , preStoreConfig_dir = storeRootDir
          }

-- * Operations

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

  runOpArgsIO WorkerOp_AddToStore $ \yield -> do
    yield $ Data.Serialize.Put.runPut $ do
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
  runOpArgs WorkerOp_AddTextToStore $ do
    putText name
    putText text
    putPaths storeDir references'
  sockGetPath

addSignatures :: StorePath -> [ByteString] -> MonadStore ()
addSignatures p signatures = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs WorkerOp_AddSignatures $ do
    putPath storeDir p
    putByteStrings signatures

addIndirectRoot :: StorePath -> MonadStore ()
addIndirectRoot pn = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs WorkerOp_AddIndirectRoot $ putPath storeDir pn

-- | Add temporary garbage collector root.
--
-- This root is removed as soon as the client exits.
addTempRoot :: StorePath -> MonadStore ()
addTempRoot pn = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs WorkerOp_AddTempRoot $ putPath storeDir pn

-- | Build paths if they are an actual derivations.
--
-- If derivation output paths are already valid, do nothing.
buildPaths :: HashSet StorePath -> BuildMode -> MonadStore ()
buildPaths ps bm = do
  storeDir <- getStoreDir
  Control.Monad.void $ simpleOpArgs WorkerOp_BuildPaths $ do
    putPaths storeDir ps
    putInt $ fromEnum bm

buildDerivation
  :: StorePath
  -> Derivation StorePath Text
  -> BuildMode
  -> MonadStore BuildResult
buildDerivation p drv buildMode = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_BuildDerivation $ do
    putPath storeDir p
    putDerivation storeDir drv
    putEnum buildMode
    -- XXX: reason for this is unknown
    -- but without it protocol just hangs waiting for
    -- more data. Needs investigation.
    -- Intentionally the only warning that should pop-up.
    putInt (0 :: Int)

  getSocketIncremental get

-- | Delete store paths
deleteSpecific
 :: HashSet StorePath -- ^ Paths to delete
 -> MonadStore GCResult
deleteSpecific paths = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_CollectGarbage $ do
    putEnum GCAction_DeleteSpecific
    putPaths storeDir paths
    putBool False -- ignoreLiveness
    putInt (maxBound :: Word64) -- maxFreedBytes
    putInt (0::Int)
    putInt (0::Int)
    putInt (0::Int)
  getSocketIncremental $ do
    gcResult_deletedPaths <- getPathsOrFail storeDir
    gcResult_bytesFreed <- getInt
    -- TODO: who knows
    _ :: Int <- getInt
    pure GCResult{..}

ensurePath :: StorePath -> MonadStore ()
ensurePath pn = do
  storeDir <- getStoreDir
  Control.Monad.void
    $ simpleOpArgs WorkerOp_EnsurePath
    $ putPath storeDir pn

-- | Find garbage collector roots.
findRoots :: MonadStore (Map ByteString StorePath)
findRoots = do
  runOp WorkerOp_FindRoots
  sd  <- getStoreDir
  res <-
    getSocketIncremental
    $ getMany
    $ (,)
      <$> getByteString
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
  simpleOpArgs WorkerOp_IsValidPath $ putPath storeDir p

-- | Query valid paths from set, optionally try to use substitutes.
queryValidPaths
  :: HashSet StorePath   -- ^ Set of `StorePath`s to query
  -> SubstituteMode      -- ^ Try substituting missing paths when `True`
  -> MonadStore (HashSet StorePath)
queryValidPaths ps substitute = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_QueryValidPaths $ do
    putPaths storeDir ps
    putBool $ substitute == SubstituteMode_DoSubstitute
  sockGetPaths

queryAllValidPaths :: MonadStore (HashSet StorePath)
queryAllValidPaths = do
  runOp WorkerOp_QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: HashSet StorePath -> MonadStore (HashSet StorePath)
querySubstitutablePaths ps = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_QuerySubstitutablePaths $ putPaths storeDir ps
  sockGetPaths

queryPathInfoUncached :: StorePath -> MonadStore (Metadata StorePath)
queryPathInfoUncached path = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_QueryPathInfo $ do
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

  sigStrings       <- fmap Data.Text.Encoding.decodeUtf8 <$> sockGetStrings
  caString         <- Data.Text.Encoding.decodeUtf8 <$> sockGetStr

  let
      sigs = case
               Data.Set.fromList
               <$> mapM System.Nix.Signature.parseNarSignature sigStrings
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
  runOpArgs WorkerOp_QueryReferrers $ putPath storeDir p
  sockGetPaths

queryValidDerivers :: StorePath -> MonadStore (HashSet StorePath)
queryValidDerivers p = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_QueryValidDerivers $ putPath storeDir p
  sockGetPaths

queryDerivationOutputs :: StorePath -> MonadStore (HashSet StorePath)
queryDerivationOutputs p = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_QueryDerivationOutputs $ putPath storeDir p
  sockGetPaths

queryDerivationOutputNames :: StorePath -> MonadStore (HashSet StorePath)
queryDerivationOutputNames p = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_QueryDerivationOutputNames $ putPath storeDir p
  sockGetPaths

queryPathFromHashPart :: StorePathHashPart -> MonadStore StorePath
queryPathFromHashPart storePathHash = do
  runOpArgs WorkerOp_QueryPathFromHashPart
    $ putText
    $ System.Nix.StorePath.storePathHashPartToText storePathHash
  sockGetPath

queryMissing
  :: (HashSet StorePath)
  -> MonadStore Missing
queryMissing ps = do
  storeDir <- getStoreDir
  runOpArgs WorkerOp_QueryMissing $ putPaths storeDir ps

  missingWillBuild      <- sockGetPaths
  missingWillSubstitute <- sockGetPaths
  missingUnknownPaths   <- sockGetPaths
  missingDownloadSize  <- sockGetInt
  missingNarSize       <- sockGetInt

  pure Missing{..}

optimiseStore :: MonadStore ()
optimiseStore = Control.Monad.void $ simpleOp WorkerOp_OptimiseStore

syncWithGC :: MonadStore ()
syncWithGC = Control.Monad.void $ simpleOp WorkerOp_SyncWithGC

-- returns True on errors
verifyStore :: CheckMode -> RepairMode -> MonadStore Bool
verifyStore check repair = simpleOpArgs WorkerOp_VerifyStore $ do
  putBool $ check == CheckMode_DoCheck
  putBool $ repair == RepairMode_DoRepair
