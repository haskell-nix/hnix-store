module System.Nix.Store.Remote.Client.Core
  ( Run
  , greetServer
  , doReq
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.DList (DList)
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
  ( ReplySError(ReplySError_PrimGet)
  , bool
  , buildResult
  , gcResult
  , gcRoot
  , hashSet
  , int
  , maybePathMetadata
  , mapErrorS
  , mapS
  , missing
  , opSuccess
  , protoVersion
  , storePath
  , storePathName
  , storeRequest
  , text
  , trustedFlag
  , workerMagic
  )

import System.Nix.Store.Remote.Types.Handshake (ClientHandshakeOutput(..))
import System.Nix.Store.Remote.Types.Logger (Logger)
import System.Nix.Store.Remote.Types.NoReply (NoReply(..))
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion(..))
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))
import System.Nix.Store.Remote.Types.WorkerMagic (WorkerMagic(..))

import Data.ByteString qualified
import Network.Socket.ByteString qualified

type Run m a = m (Either RemoteStoreError a, DList Logger)

-- | Perform @StoreRequest@
doReq
  :: forall m a
   . ( MonadIO m
     , MonadRemoteStore m
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
          $ storeRequest storeDir pv
      )
      (Some x)

    case x of
      AddToStore {} -> do

        ms <- takeNarSource
        case ms of
          Just (stream :: NarSource IO) -> do
            soc <- getStoreSocket
            liftIO
              $ stream
              $ Network.Socket.ByteString.sendAll soc
          Nothing ->
            throwError
              RemoteStoreError_NoNarSourceProvided
        processOutput
        sockGetS $ mapErrorS (RemoteStoreError_SerializerReply . ReplySError_PrimGet) $ storePath storeDir

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

      AddTextToStore {} -> do
        processOutput
        sockGetS $ mapErrorS (RemoteStoreError_SerializerReply . ReplySError_PrimGet) $ storePath storeDir

      AddSignatures _path _sigs -> do
        processOutput
        sockGetS $ mapErrorS RemoteStoreError_SerializerReply opSuccess

      AddIndirectRoot _path -> do
        processOutput
        sockGetS $ mapErrorS RemoteStoreError_SerializerReply opSuccess

      AddTempRoot _path -> do
        processOutput
        sockGetS $ mapErrorS RemoteStoreError_SerializerReply opSuccess

      BuildPaths {} -> do
        processOutput
        sockGetS $ mapErrorS RemoteStoreError_SerializerReply opSuccess

      BuildDerivation {} -> do
        processOutput
        sockGetS
          $ mapErrorS
              RemoteStoreError_SerializerReply
              $ buildResult storeDir pv

      CollectGarbage _gcOpts -> do
        processOutput
        sockGetS
          $ mapErrorS
              RemoteStoreError_SerializerReply
              $ gcResult storeDir

      EnsurePath _path -> do
        processOutput
        sockGetS $ mapErrorS RemoteStoreError_SerializerReply opSuccess

      FindRoots -> do
        processOutput
        sockGetS
          $ mapErrorS RemoteStoreError_SerializerReply
          $ mapS
              gcRoot
              $ mapErrorS
                  ReplySError_PrimGet
                  (storePath storeDir)

      IsValidPath _path -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              bool

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

      QueryValidPaths {} -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ hashSet (storePath storeDir)

      QueryAllValidPaths -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ hashSet (storePath storeDir)

      QuerySubstitutablePaths {} -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ hashSet (storePath storeDir)


      QueryPathInfo {} -> do
        processOutput
        sockGetS
          $ mapErrorS
              RemoteStoreError_SerializerReply
              $ maybePathMetadata storeDir

      QueryReferrers _path -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ hashSet (storePath storeDir)

      QueryValidDerivers _path -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ hashSet (storePath storeDir)

      QueryDerivationOutputs _path -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ hashSet (storePath storeDir)

      QueryDerivationOutputNames _path -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ hashSet storePathName

      QueryPathFromHashPart _pathHashPart -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              $ storePath storeDir

      QueryMissing _derivedPathSet -> do
        processOutput
        sockGetS
          $ mapErrorS
              RemoteStoreError_SerializerReply
              $ missing storeDir

      OptimiseStore -> do
        processOutput
        sockGetS $ mapErrorS RemoteStoreError_SerializerReply opSuccess

      SyncWithGC -> do
        processOutput
        sockGetS $ mapErrorS RemoteStoreError_SerializerReply opSuccess

      VerifyStore {} -> do
        processOutput
        sockGetS
          $ mapErrorS
              (RemoteStoreError_SerializerReply . ReplySError_PrimGet)
              bool

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

  when (daemonVersion < ProtoVersion 1 10)
    $ throwError RemoteStoreError_ClientVersionTooOld

  pv <- getProtoVersion
  sockPutS protoVersion pv

  let leastCommonVersion = min daemonVersion pv

  when (leastCommonVersion >= ProtoVersion 1 14)
    $ sockPutS int (0 :: Int) -- affinity, obsolete

  when (leastCommonVersion >= ProtoVersion 1 11) $ do
    sockPutS
      (mapErrorS RemoteStoreError_SerializerPut bool)
      False -- reserveSpace, obsolete

  daemonNixVersion <- if leastCommonVersion >= ProtoVersion 1 33
    then do
      -- If we were buffering I/O, we would flush the output here.
      txtVer <-
        sockGetS
          $ mapErrorS
              RemoteStoreError_SerializerGet
              text
      pure $ Just txtVer
    else pure Nothing

  remoteTrustsUs <- if leastCommonVersion >= ProtoVersion 1 35
    then do
      sockGetS
        $ mapErrorS RemoteStoreError_SerializerHandshake trustedFlag
    else pure Nothing

  setProtoVersion leastCommonVersion
  processOutput

  pure ClientHandshakeOutput
    { clientHandshakeOutputNixVersion = daemonNixVersion
    , clientHandshakeOutputTrust = remoteTrustsUs
    , clientHandshakeOutputLeastCommonVersion = leastCommonVersion
    , clientHandshakeOutputServerVersion = daemonVersion
    }
