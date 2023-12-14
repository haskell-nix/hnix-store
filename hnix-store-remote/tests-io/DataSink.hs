module DataSink

( DataSink(..)
, dataSinkResult
, dataSinkWriter
, newDataSink
)

where

import Data.ByteString (ByteString)

import Control.Monad.ST
import Data.STRef

-- | Basic data sink for testing
newtype DataSink = DataSink (STRef RealWorld ByteString)

newDataSink :: IO DataSink
newDataSink = DataSink <$> (stToIO . newSTRef) mempty

dataSinkWriter :: DataSink -> (ByteString -> IO())
dataSinkWriter (DataSink stref) chunk = stToIO (modifySTRef stref (chunk <>))

dataSinkResult :: DataSink -> IO ByteString
dataSinkResult (DataSink stref) = (stToIO . readSTRef) stref
