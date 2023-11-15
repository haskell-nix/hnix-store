{-# LANGUAGE NumericUnderscores #-}
module SerializeSpec where

import Prelude hiding (putText)
import Data.Fixed (Uni)
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Putter, runPut)
import Data.Time (NominalDiffTime, UTCTime)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances

import System.Nix.StorePath (StoreDir, StorePath)
import System.Nix.Store.Remote.Serialize
import System.Nix.Store.Remote.Serialize.Prim

import qualified Data.HashSet
import qualified Data.Time.Clock.POSIX

roundTrip :: (Eq a, Show a) => Putter a -> Get a -> a -> Property
roundTrip p g a = res === Right a
  where res = runGet g (runPut (p a))

-- * Prim
-- ** Int

prop_int :: Int -> Property
prop_int = roundTrip putInt getInt

-- ** Bool

prop_bool :: Bool -> Property
prop_bool = roundTrip putBool getBool

-- ** UTCTime

prop_time :: Int -> Property
prop_time =
 roundTrip
  (putTime . Data.Time.Clock.POSIX.posixSecondsToUTCTime . toSeconds)
  (fromSeconds . Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds <$> getTime)
  where
    -- scale to seconds and back
    toSeconds :: Int -> NominalDiffTime
    toSeconds n = realToFrac (toEnum n :: Uni)
    fromSeconds :: NominalDiffTime -> Int
    fromSeconds = (fromEnum :: Uni -> Int) . realToFrac

-- ** Combinators

prop_many :: [Int] -> Property
prop_many = roundTrip (putMany putInt) (getMany getInt)

-- ** ByteString

prop_bytestring :: ByteString -> Property
prop_bytestring = roundTrip putByteString getByteString

prop_bytestrings :: [ByteString] -> Property
prop_bytestrings = roundTrip putByteStrings getByteStrings

-- ** Text

prop_text :: Text -> Property
prop_text = roundTrip putText getText

prop_texts :: [Text] -> Property
prop_texts = roundTrip putTexts getTexts

-- ** StorePath

prop_path :: StoreDir -> StorePath -> Property
prop_path = \sd ->
  roundTrip
    (putPath sd)
    (fromRight undefined <$> getPath sd)

prop_paths :: StoreDir -> HashSet StorePath -> Property
prop_paths = \sd ->
  roundTrip
    (putPaths sd)
    (Data.HashSet.map (fromRight undefined) <$> getPaths sd)
