module System.Nix.Store.Remote.Client.Core
  ( Run
  , greetServer
  , doReq
  ) where

import Algebra.PartialOrd (leq)
import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import Data.DList (DList)
import Data.Set qualified
import Data.Some (Some(Some))
import Data.Word (Word64)
import Network.Socket (Socket)
import System.Nix.Nar (NarSource)
import System.Nix.StorePath.Metadata (Metadata(..))
import System.Nix.Store.Remote.Logger (processOutput)
import System.Nix.Store.Remote.MonadStore
  ( MonadRemoteStore(..)
  , RemoteStoreError(..)
  )
import System.Nix.Store.Remote.Socket (sockPutS, sockGetS)
import System.Nix.Store.Remote.Serializer
  ( bool
  , int
  , mapErrorS
  , protoFeatures
  , protoVersion
  , validPathInfo
  , storeRequest
  , text
  , trustedFlag
  , workerMagic
  )
import System.Nix.Store.Remote.Types.Handshake (ClientHandshakeOutput(..))
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.NoReply (NoReply(..))
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion(..), minVersionNumber)
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.StoreReply (StoreReply(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))

import Data.ByteString qualified
import Network.Socket.ByteString qualified

type Run m a = m (Either RemoteStoreError a, DList Logger)

-- | Perform @StoreRequest@
doReq
  :: forall m a
   . ( MonadIO m
     , MonadRemoteStore m
     , StoreReply a
     , Show a
     )
  => StoreRequest a
  -> m a
doReq = \case
  x -> do
    storeDir <- getStoreDir
    pv <- getProtoVersion
    sockPutS
      (mapErrorS
        RemoteStoreError_SerializerRequest
          (storeRequest storeDir pv)
      )
      (Some x)

    case x of
      AddToStore {} -> do
        ms <- takeNarSource
        soc <- getStoreSocket
        case ms of
          Just (stream :: NarSource IO) ->
            liftIO $ writeFramedNarSource stream soc
          Nothing ->
            throwError
              RemoteStoreError_NoNarSourceProvided
        processOutput
        -- New protocol returns ValidPathInfo (path + metadata)
        (path, _metadata) <- sockGetS
          $ mapErrorS RemoteStoreError_SerializerGet
          $ validPathInfo storeDir
        pure path

      AddToStoreScanning {} -> do
        ms <- takeNarSource
        soc <- getStoreSocket
        case ms of
          Just (stream :: NarSource IO) ->
            liftIO $ writeFramedNarSource stream soc
          Nothing ->
            throwError
              RemoteStoreError_NoNarSourceProvided
        processOutput
        (path, _metadata) <- sockGetS
          $ mapErrorS RemoteStoreError_SerializerGet
          $ validPathInfo storeDir
        pure path

      AddToStoreNar _ meta _ _ -> do
        let narBytes = maybe 0 id $ metadataNarBytes meta
        maybeDataSource <- takeDataSource
        soc <- getStoreSocket
        case maybeDataSource of
          Nothing ->
            if narBytes == 0 then writeFramedSource (const (pure Nothing)) soc 0
            else throwError RemoteStoreError_NoDataSourceProvided
          Just dataSource -> do
            writeFramedSource dataSource soc narBytes
        processOutput
        pure NoReply

      NarFromPath _ -> do
        maybeSink <- getDataSink
        sink <- case maybeSink of
          Nothing -> throwError RemoteStoreError_NoDataSinkProvided
          Just sink -> pure sink
        clearDataSink
        maybeNarSize <- getDataSinkSize
        narSize <- case maybeNarSize of
          Nothing -> throwError RemoteStoreError_NoDataSinkSizeProvided
          Just narSize -> pure narSize
        clearDataSinkSize
        soc <- getStoreSocket
        processOutput
        copyToSink sink narSize soc
        pure NoReply

      _ -> do
        processOutput
        processReply

  where
    processReply = do
      storeDir <- getStoreDir
      pv <- getProtoVersion
      sockGetS
          (mapErrorS RemoteStoreError_SerializerReply
            $ getReplyS @a storeDir pv
          )

copyToSink
  :: forall m
   . ( MonadIO m
     , MonadRemoteStore m
     )
  => (ByteString -> IO()) --  ^ data sink
  -> Word64 -- ^ byte length to read
  -> Socket
  -> m ()
copyToSink sink remainingBytes soc =
  when (remainingBytes > 0) $ do
    let chunkSize = 16384
        bytesToRead = min chunkSize remainingBytes
    bytes <- liftIO $ Network.Socket.ByteString.recv soc (fromIntegral bytesToRead)
    liftIO $ sink bytes
    let nextRemainingBytes = remainingBytes - (fromIntegral . Data.ByteString.length) bytes
    copyToSink sink nextRemainingBytes soc

-- | Write a NarSource as framed data to a socket.
-- Each chunk of NAR data is prefixed with its length as a
-- little-endian u64, terminated by a zero-length frame.
writeFramedNarSource :: NarSource IO -> Socket -> IO ()
writeFramedNarSource narSource sock = do
  narSource sendChunk
  sendWord64le sock 0 -- terminator
  where
    sendChunk :: ByteString -> IO ()
    sendChunk bs = do
      let len = Data.ByteString.length bs
      when (len > 0) $ do
        sendWord64le sock (fromIntegral len)
        Network.Socket.ByteString.sendAll sock bs

    sendWord64le :: Socket -> Word64 -> IO ()
    sendWord64le s w =
      Network.Socket.ByteString.sendAll s
        $ Data.ByteString.pack
            [ fromIntegral (shiftR w (i * 8))
            | i <- [0..7]
            ]

writeFramedSource
  :: forall m
   . ( MonadIO m
     , MonadRemoteStore m
     )
  => (Word64 -> IO(Maybe ByteString))
  -> Socket
  -> Word64
  -> m ()
writeFramedSource dataSource soc remainingBytes = do
  let chunkSize = 16384
  maybeBytes <- liftIO $ dataSource chunkSize
  case maybeBytes of
    Nothing -> do
      unless (remainingBytes == 0) $ throwError RemoteStoreError_DataSourceExhausted
      let eof :: Word64 = 0
      sockPutS int eof
    Just bytes -> do
      let bytesInChunk = fromIntegral $ Data.ByteString.length bytes
      when (bytesInChunk > chunkSize || bytesInChunk > remainingBytes) $ throwError RemoteStoreError_DataSourceReadTooLarge
      when (bytesInChunk == 0) $ throwError RemoteStoreError_DataSourceZeroLengthRead
      sockPutS int bytesInChunk
      liftIO
        $ Network.Socket.ByteString.sendAll soc bytes
      let nextRemainingBytes = remainingBytes - bytesInChunk
      writeFramedSource dataSource soc nextRemainingBytes

greetServer
  :: MonadRemoteStore m
  => m ClientHandshakeOutput
greetServer = do
  sockPutS
    (mapErrorS
      RemoteStoreError_SerializerHandshake
      workerMagic
    )
    WorkerMagic_One

  magic <-
    sockGetS
    $ mapErrorS
        RemoteStoreError_SerializerHandshake
        workerMagic

  unless
    (magic == WorkerMagic_Two)
    $ throwError RemoteStoreError_WorkerMagic2Mismatch

  daemonVersion <- sockGetS protoVersion

  when (not (ProtoVersion 1 37 mempty `leq` daemonVersion))
    $ throwError RemoteStoreError_ClientVersionTooOld

  pv <- getProtoVersion
  sockPutS protoVersion pv

  let leastCommonVersion = minVersionNumber daemonVersion pv

  -- Feature exchange (>= 1.38)
  negotiatedFeatures <- if ProtoVersion 1 38 mempty `leq` leastCommonVersion
    then do
      sockPutS
        (mapErrorS RemoteStoreError_SerializerPut protoFeatures)
        (protoVersion_features pv)
      daemonFeatures <- sockGetS
        $ mapErrorS RemoteStoreError_SerializerGet protoFeatures
      pure $ Data.Set.intersection daemonFeatures (protoVersion_features pv)
    else pure mempty

  let leastCommonVersionWithFeatures = leastCommonVersion { protoVersion_features = negotiatedFeatures }

  setProtoVersion leastCommonVersionWithFeatures

  -- postHandshake: affinity (obsolete), reserveSpace (obsolete)
  sockPutS int (0 :: Int) -- affinity, obsolete
  sockPutS (mapErrorS RemoteStoreError_SerializerPut bool) False -- reserveSpace, obsolete

  -- If we were buffering I/O, we would flush the output here.

  daemonNixVersion <- Just <$>
    sockGetS (mapErrorS RemoteStoreError_SerializerGet text)

  remoteTrustsUs <-
    sockGetS (mapErrorS RemoteStoreError_SerializerHandshake trustedFlag)

  processOutput

  pure ClientHandshakeOutput
    { clientHandshakeOutputNixVersion = daemonNixVersion
    , clientHandshakeOutputTrust = remoteTrustsUs
    , clientHandshakeOutputLeastCommonVersion = leastCommonVersionWithFeatures
    , clientHandshakeOutputServerVersion = daemonVersion
    }
