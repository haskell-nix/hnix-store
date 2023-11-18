
module ContentAddress where

import Test.Tasty.QuickCheck
import System.Nix.ContentAddress (ContentAddress)

import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Encoding

import qualified System.Nix.ContentAddress

prop_caAddrRoundTrip :: ContentAddress -> Property
prop_caAddrRoundTrip = \caAddr ->
  Data.Attoparsec.ByteString.Char8.parseOnly
    System.Nix.ContentAddress.contentAddressParser
    ( Data.Text.Encoding.encodeUtf8
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    $ System.Nix.ContentAddress.contentAddressBuilder
       caAddr
    )
  === pure caAddr

