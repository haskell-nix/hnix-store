
module Util where

import           Test.Tasty.QuickCheck
import           Data.Text.Arbitrary

import           System.Nix.Store.Remote.Util

prop_TextToBSLRoundtrip x =
    bslToText (textToBSL x) === x

prop_TextToBSRoundtrip x =
    bsToText (textToBS x) === x
