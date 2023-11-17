
module ContentAddressableAddress where

import Test.Tasty.QuickCheck
import System.Nix.StorePath (ContentAddressableAddress, contentAddressableAddressBuilder, contentAddressableAddressParser)

import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Encoding

prop_caAddrRoundTrip :: ContentAddressableAddress -> Property
prop_caAddrRoundTrip = \caAddr ->
  Data.Attoparsec.ByteString.Char8.parseOnly contentAddressableAddressParser
   ( Data.Text.Encoding.encodeUtf8
   . Data.Text.Lazy.toStrict
   . Data.Text.Lazy.Builder.toLazyText
   $ contentAddressableAddressBuilder caAddr
   )
  === pure caAddr

