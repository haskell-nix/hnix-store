{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StorePath where

import qualified Data.Attoparsec.Text

import           Test.Tasty.QuickCheck

import           System.Nix.StorePath
import           Arbitrary

-- | Test that Nix(OS) like paths roundtrip
prop_storePathRoundtrip :: NixLike -> NixLike -> Property
prop_storePathRoundtrip (_ :: NixLike) (NixLike x) =
  (parsePath "/nix/store" $ storePathToRawFilePath x) === Right x

-- | Test that any `StorePath` roundtrips
prop_storePathRoundtrip' :: StorePath -> Property
prop_storePathRoundtrip' x =
  parsePath (storePathRoot x) (storePathToRawFilePath x) === Right x

prop_storePathRoundtripParser :: NixLike -> NixLike -> Property
prop_storePathRoundtripParser (_ :: NixLike) (NixLike x) =
  (Data.Attoparsec.Text.parseOnly (pathParser $ storePathRoot x) $ storePathToText x) === Right x

prop_storePathRoundtripParser' :: StorePath -> Property
prop_storePathRoundtripParser' x =
  Data.Attoparsec.Text.parseOnly (pathParser $ storePathRoot x) (storePathToText x) === Right x
