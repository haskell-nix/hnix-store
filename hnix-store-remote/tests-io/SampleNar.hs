{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module SampleNar

( SampleNar(..)
, buildDataSource
, sampleNar0
, encodeNar
)

where

import Crypto.Hash ( Digest, SHA256, hashInit, hashUpdate, hashFinalize )
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum((:=>)))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (UTCTime(..))
import System.Nix.Hash (HashAlgo(HashAlgo_SHA256))
import System.Nix.StorePath (StorePath, parsePath)
import System.Nix.StorePath.Metadata (Metadata(..), StorePathTrust(..))

import Control.Monad.ST
import Data.Default.Class
import Data.STRef
import Data.Word

import qualified Data.ByteString
import qualified System.Nix.Nar

-- | Sample data for an AddToStoreNar operation
data SampleNar
  = SampleNar
  { sampleNar_fileData :: !ByteString -- ^ Contents of the file to be put in the store
  , sampleNar_narData :: !ByteString -- ^ The file data serialized as a NAR
  , sampleNar_storePath :: !StorePath
  , sampleNar_metadata :: !(Metadata StorePath)
  }

sampleNar0 :: IO SampleNar
sampleNar0 = do
  let sampleNar_fileData = "hello"
  sampleNar_narData <- encodeNar sampleNar_fileData
  let sampleNar_metadata = Metadata
        { metadataDeriverPath = Just $ forceParsePath "/nix/store/g2mxdrkwr1hck4y5479dww7m56d1x81v-hello-2.12.1.drv"
        , metadataNarHash = sha256 sampleNar_narData
        , metadataReferences = mempty
        , metadataRegistrationTime = UTCTime (fromOrdinalDate 1980 1) 0
        , metadataNarBytes = Just ((fromIntegral . Data.ByteString.length) sampleNar_narData)
        , metadataTrust = BuiltElsewhere
        , metadataSigs = mempty
        , metadataContentAddress = Nothing
        }
      sampleNar_storePath = forceParsePath "/nix/store/00000lj3clbkc0aqvjjzfa6slp4zdvlj-hello-2.12.1"
  pure SampleNar{..}

buildDataSource :: SampleNar -> IO(Word64 -> IO (Maybe ByteString))
buildDataSource SampleNar{sampleNar_narData} = dataSourceFromByteString sampleNar_narData

dataSourceFromByteString :: ByteString -> IO (Word64 -> IO(Maybe ByteString))
dataSourceFromByteString bs = do
  posRef <- stToIO $ newSTRef (0::Word64)
  let len = fromIntegral $ Data.ByteString.length bs
      outFn chunkSize = do
        pos <- stToIO $ readSTRef posRef
        if pos >= len then pure Nothing
        else do
          let bs' = Data.ByteString.drop (fromIntegral pos) bs
              bs'' = Data.ByteString.take (fromIntegral chunkSize) bs'
              takenLen = fromIntegral $ Data.ByteString.length bs''
          stToIO $ modifySTRef posRef (takenLen +)
          pure $ Just bs''
  pure outFn

forceParsePath :: ByteString -> StorePath
forceParsePath path = case parsePath def path of
  Left err -> error $ mconcat [ "forceParsePath failed: ", show err ]
  Right x -> x
sha256 :: ByteString -> DSum HashAlgo Digest
sha256 bs = HashAlgo_SHA256 :=> hashFinalize (hashUpdate (hashInit @SHA256) bs)

encodeNar :: ByteString -> IO ByteString
encodeNar bytes = do
  ref <- stToIO $ newSTRef mempty
  let accumFn chunk = do
       stToIO $ modifySTRef ref (<> chunk)
  System.Nix.Nar.dumpString bytes accumFn
  stToIO $ readSTRef ref

