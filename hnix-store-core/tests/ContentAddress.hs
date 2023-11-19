module ContentAddress where

import Test.Tasty.QuickCheck
import System.Nix.ContentAddress (ContentAddress)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.Builder

import qualified System.Nix.ContentAddress

prop_caAddrRoundTrip :: ContentAddress -> Property
prop_caAddrRoundTrip = \caAddr ->
  Data.Attoparsec.Text.Lazy.parseOnly
    System.Nix.ContentAddress.contentAddressParser
      (Data.Text.Lazy.Builder.toLazyText
        (System.Nix.ContentAddress.contentAddressBuilder caAddr))
  === pure caAddr

