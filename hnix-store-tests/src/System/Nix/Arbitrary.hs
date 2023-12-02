module System.Nix.Arbitrary where

import Data.ByteString.Arbitrary ()
import Data.HashSet.Arbitrary ()
import Data.Text.Arbitrary ()
import Data.Vector.Arbitrary ()

import System.Nix.Arbitrary.Base ()
import System.Nix.Arbitrary.Build ()
import System.Nix.Arbitrary.ContentAddress ()
import System.Nix.Arbitrary.Derivation ()
import System.Nix.Arbitrary.DerivedPath ()
import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.Signature ()
import System.Nix.Arbitrary.Store.Types ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.Arbitrary.StorePath.Metadata ()
