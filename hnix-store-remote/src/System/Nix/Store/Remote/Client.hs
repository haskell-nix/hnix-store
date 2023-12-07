module System.Nix.Store.Remote.Client
  ( simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
  , addToStore
  , buildDerivation
  , isValidPath
  , module System.Nix.Store.Remote.Client.Core
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Serialize.Put (Put, runPut)
import Data.Some (Some)
import Data.Text (Text)

import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.Derivation (Derivation)
import System.Nix.Hash (HashAlgo(..))
import System.Nix.Nar (NarSource)
import System.Nix.StorePath (StorePath, StorePathName)
import System.Nix.Store.Remote.Logger (processOutput)
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Socket (sockPutS, sockGetS)
import System.Nix.Store.Remote.Serializer (bool, enum, mapErrorS)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.WorkerOp (WorkerOp)
import System.Nix.Store.Remote.Client.Core
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))

import qualified Data.ByteString
import qualified Network.Socket.ByteString

simpleOp
  :: MonadRemoteStore m
  => WorkerOp
  -> m Bool
simpleOp op = simpleOpArgs op $ pure ()

simpleOpArgs
  :: MonadRemoteStore m
  => WorkerOp
  -> Put
  -> m Bool
simpleOpArgs op args = do
  runOpArgs op args
  errored <- gotError
  if errored
  then throwError RemoteStoreError_OperationFailed
  else sockGetS $ mapErrorS RemoteStoreError_SerializerGet bool

runOp
  :: MonadRemoteStore m
  => WorkerOp
  -> m ()
runOp op = runOpArgs op $ pure ()

runOpArgs
  :: MonadRemoteStore m
  => WorkerOp
  -> Put
  -> m ()
runOpArgs op args =
  runOpArgsIO
    op
    (\encode -> encode $ runPut args)

runOpArgsIO
  :: MonadRemoteStore m
  => WorkerOp
  -> ((Data.ByteString.ByteString -> m ())
       -> m ()
     )
  -> m ()
runOpArgsIO op encoder = do
  sockPutS (mapErrorS RemoteStoreError_SerializerPut enum) op

  soc <- getStoreSocket
  encoder (liftIO . Network.Socket.ByteString.sendAll soc)

  processOutput

-- | Add `NarSource` to the store
addToStore
  :: MonadRemoteStore m
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource IO         -- ^ Provide nar stream
  -> FileIngestionMethod  -- ^ Add target directory recursively
  -> Some HashAlgo        -- ^
  -> RepairMode           -- ^ Only used by local store backend
  -> m StorePath
addToStore name source method hashAlgo repair = do
  Control.Monad.when
    (repair == RepairMode_DoRepair)
    $ throwError RemoteStoreError_RapairNotSupportedByRemoteStore

  setNarSource source
  doReq (AddToStore name method hashAlgo repair)

buildDerivation
  :: MonadRemoteStore m
  => StorePath
  -> Derivation StorePath Text
  -> BuildMode
  -> m BuildResult
buildDerivation a b c = doReq (BuildDerivation a b c)

isValidPath :: MonadRemoteStore m => StorePath -> m Bool
isValidPath = doReq . IsValidPath
