{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module System.Nix.Store.Remote.Server
  ( runProxyDaemon
  , WorkerHelper
  )
  where

import Control.Concurrent.Classy.Async
import Control.Monad (join, void, when)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class (Default(def))
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Text (Text)
import Data.Void (Void, absurd)
import Data.Word (Word32)
import Network.Socket (Socket, accept, close, listen, maxListenQueue)
import System.Nix.Nar (NarSource)
import System.Nix.Store.Remote.Client (Run, doReq)
import System.Nix.Store.Remote.Serializer (LoggerSError, mapErrorS, storeRequest, workerMagic, protoVersion, int, logger, text, trustedFlag)
import System.Nix.Store.Remote.Socket
import System.Nix.Store.Remote.Types.StoreRequest as R
import System.Nix.Store.Remote.Types.StoreReply
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion(..))
import System.Nix.Store.Remote.Types.Logger (BasicError(..), ErrorInfo, Logger(..))
import System.Nix.Store.Remote.MonadStore (MonadRemoteStore(..), WorkerError(..), WorkerException(..), RemoteStoreError(..), RemoteStoreT, runRemoteStoreT)
import System.Nix.Store.Remote.Types.Handshake (ServerHandshakeInput(..), ServerHandshakeOutput(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))
import qualified Data.Some
import qualified Data.Text
import qualified Data.Text.IO
import qualified System.Timeout
import qualified Network.Socket.ByteString

type WorkerHelper m
  = forall a
  . ( Show a
    , StoreReply a
    )
  => RemoteStoreT m a
  -> Run m a

chatty :: Bool
chatty = False

dbg :: MonadIO m => Text -> m ()
dbg = when chatty . liftIO . Data.Text.IO.putStrLn

-- | Run an emulated nix daemon on given socket address.
-- The deamon will close when the continuation returns.
runProxyDaemon
  :: forall m a
  . ( MonadIO m
    , MonadConc m
    )
  => WorkerHelper m
  -> RemoteStoreT m ()
  -> Socket
  -> m a
  -> m a
runProxyDaemon workerHelper postGreet lsock k = do
  liftIO $ listen lsock maxListenQueue

  dbg "listening"

  let listener :: m Void
      listener = do
        (sock, _) <- liftIO $ accept lsock
        dbg "accepting"

        -- TODO: this, but without the space leak
        fmap fst
          $ concurrently listener
          $ processConnection workerHelper postGreet sock

  either absurd id <$> race listener k

-- | "main loop" of the daemon for a single connection.
--
-- this function should take care to not throw errors from client connections.
processConnection
  :: forall m
  .  MonadIO m
  => WorkerHelper m
  -> RemoteStoreT m ()
  -> Socket
  -> m ()
processConnection workerHelper postGreet sock = do
  ~() <- void $ runRemoteStoreT sock $ do

    ServerHandshakeOutput{..}
      <- greet
          ServerHandshakeInput
          { serverHandshakeInputNixVersion = "nixVersion (hnix-store-remote)"
          , serverHandshakeInputOurVersion = def
          , serverHandshakeInputTrust = Nothing
          }

    setProtoVersion serverHandshakeOutputLeastCommonVersion

    tunnelLogger <- liftIO $ newTunnelLogger
    -- Send startup error messages to the client.
    startWork tunnelLogger

    -- TODO: do we need auth at all? probably?
    -- If we can't accept clientVersion, then throw an error *here* (not above).
    --authHook(*store);
    stopWork tunnelLogger

    -- so we can set store dir
    postGreet

    let perform
          :: ( Show a
             , StoreReply a
             )
          => StoreRequest a
          -> RemoteStoreT m ()
        perform req = do

          special <- case req of
            AddToStore {} -> do
              let proxyNarSource :: NarSource IO
                  proxyNarSource f =
                    liftIO
                      (System.Timeout.timeout
                         1000000
                         (Network.Socket.ByteString.recv sock 8)
                      )
                    >>= \case
                      Nothing -> pure ()
                      Just x -> f x >> proxyNarSource f

              pure $ setNarSource proxyNarSource
            _ -> pure $ pure ()

          res <-
            bracketLogger
              tunnelLogger
              $ lift
              $ workerHelper
              $ special >> doReq req

          case fst res of
            Left e -> throwError e
            Right reply ->
              sockPutS
                (mapErrorS
                   RemoteStoreError_SerializerReply
                   $ getReplyS
                )
                reply

    -- Process client requests.
    let loop = do
          someReq <-
            sockGetS
              $ mapErrorS
                  RemoteStoreError_SerializerRequest
                  storeRequest

          -- have to be explicit here
          -- because otherwise GHC can't conjure Show a, StoreReply a
          -- out of thin air
          () <- Data.Some.withSome someReq $ \case
            r@AddToStore {} -> perform r
            r@AddTextToStore {} -> perform r
            r@AddSignatures {} -> perform r
            r@AddTempRoot {} -> perform r
            r@AddIndirectRoot {} -> perform r
            r@BuildDerivation {} -> perform r
            r@BuildPaths {} -> perform r
            r@CollectGarbage {} -> perform r
            r@EnsurePath {} -> perform r
            r@FindRoots {} -> perform r
            r@IsValidPath {} -> perform r
            r@QueryValidPaths {} -> perform r
            r@QueryAllValidPaths {} -> perform r
            r@QuerySubstitutablePaths {} -> perform r
            r@QueryPathInfo {} -> perform r
            r@QueryReferrers {} -> perform r
            r@QueryValidDerivers {} -> perform r
            r@QueryDerivationOutputs {} -> perform r
            r@QueryDerivationOutputNames {} -> perform r
            r@QueryPathFromHashPart {} -> perform r
            r@QueryMissing {} -> perform r
            r@OptimiseStore {} -> perform r
            r@SyncWithGC {} -> perform r
            r@VerifyStore {} -> perform r
          loop
    loop

  dbg "daemon connection done"
  liftIO $ close sock

  where
    -- Exchange the greeting.
    greet
      :: MonadIO m
      => ServerHandshakeInput
      -> RemoteStoreT m ServerHandshakeOutput
    greet ServerHandshakeInput{..} = do
      magic <-
        sockGetS
        $ mapErrorS
            RemoteStoreError_SerializerHandshake
            workerMagic

      when (magic /= WorkerMagic_One)
        $ throwError
        $ RemoteStoreError_WorkerException
            WorkerException_ProtocolMismatch

      sockPutS
        (mapErrorS
          RemoteStoreError_SerializerHandshake
          workerMagic
        )
        WorkerMagic_Two

      sockPutS protoVersion serverHandshakeInputOurVersion

      clientVersion <- sockGetS protoVersion

      let leastCommonVersion = min clientVersion serverHandshakeInputOurVersion

      when (clientVersion < ProtoVersion 1 10)
        $ throwError
        $ RemoteStoreError_WorkerException
            WorkerException_ClientVersionTooOld

      when (clientVersion >= ProtoVersion 1 14) $ do
        x :: Word32 <- sockGetS int
        when (x /= 0) $ do
          -- Obsolete CPU affinity.
          _ :: Word32 <- sockGetS int
          pure ()

      when (clientVersion >= ProtoVersion 1 11) $ do
        _ :: Word32 <- sockGetS int -- obsolete reserveSpace
        pure ()

      when (clientVersion >= ProtoVersion 1 33) $ do
        sockPutS
          (mapErrorS
             RemoteStoreError_SerializerPut
             text
          )
          serverHandshakeInputNixVersion

      when (clientVersion >= ProtoVersion 1 35) $ do
        sockPutS
          (mapErrorS
             RemoteStoreError_SerializerHandshake
             trustedFlag
          )
          serverHandshakeInputTrust

      pure ServerHandshakeOutput
        { serverHandshakeOutputLeastCommonVersion = leastCommonVersion
        , serverHandshakeOutputClientVersion = clientVersion
        }

{-# WARNING _unimplemented "not yet implemented" #-}
_unimplemented :: RemoteStoreError
_unimplemented = RemoteStoreError_WorkerException $ WorkerException_Error $ WorkerError_NotYetImplemented

bracketLogger
  :: MonadRemoteStore m
  => TunnelLogger
  -> m a
  -> m a
bracketLogger tunnelLogger m = do
  startWork tunnelLogger
  a <- m
  stopWork tunnelLogger
  pure a

---

data TunnelLogger = TunnelLogger
  { _tunnelLogger_state :: IORef TunnelLoggerState
  }

data TunnelLoggerState = TunnelLoggerState
  { _tunnelLoggerState_canSendStderr :: Bool
  , _tunnelLoggerState_pendingMsgs :: [Logger]
  }

newTunnelLogger :: IO TunnelLogger
newTunnelLogger = TunnelLogger <$> newIORef (TunnelLoggerState False [])

enqueueMsg
  :: ( MonadRemoteStore m
     , MonadError LoggerSError m
     )
  => TunnelLogger
  -> Logger
  -> m ()
enqueueMsg x l = updateLogger x $ \st@(TunnelLoggerState c p) -> case c of
  True -> (st, sockPutS logger l)
  False -> (TunnelLoggerState c (l:p), pure ())

_log
  :: ( MonadRemoteStore m
     , MonadError LoggerSError m
     )
  => TunnelLogger
  -> Text
  -> m ()
_log l s = enqueueMsg l (Logger_Next s)

startWork
  :: MonadRemoteStore m
  => TunnelLogger
  -> m ()
startWork x = updateLogger x $ \(TunnelLoggerState _ p) -> (,)
  (TunnelLoggerState True []) $
  (traverse_ (sockPutS logger') $ reverse p)
  where logger' = mapErrorS RemoteStoreError_SerializerLogger logger

stopWork
  :: MonadRemoteStore m
  => TunnelLogger
  -> m ()
stopWork x = updateLogger x $ \_ -> (,)
  (TunnelLoggerState False [])
  (sockPutS (mapErrorS RemoteStoreError_SerializerLogger logger) Logger_Last)

-- | Stop sending logging and report an error.
--
-- Returns true if the the session was in a state that allowed the error to be
-- sent.
--
-- Unlike 'stopWork', this function may be called at any time to (try) to end a
-- session with an error.
_stopWorkOnError
  :: MonadRemoteStore m
  => TunnelLogger
  -> ErrorInfo
  -> m Bool
_stopWorkOnError x ex = updateLogger x $ \st ->
  case _tunnelLoggerState_canSendStderr st of
    False -> (st, pure False)
    True -> (,) (TunnelLoggerState False []) $ do
      getProtoVersion >>= \pv -> if protoVersion_minor pv >= 26
        then sockPutS logger' (Logger_Error (Right ex))
        else sockPutS logger' (Logger_Error (Left (BasicError 0 (Data.Text.pack $ show ex))))
      pure True
  where logger' = mapErrorS RemoteStoreError_SerializerLogger logger

updateLogger
  :: MonadRemoteStore m
  => TunnelLogger
  -> (TunnelLoggerState -> (TunnelLoggerState, m a))
  -> m a
updateLogger x = join . liftIO . atomicModifyIORef (_tunnelLogger_state x)
