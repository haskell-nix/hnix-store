-- due to recent generic-arbitrary
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.StorePath.Metadata where

import Data.Dependent.Sum (DSum((:=>)))
import Data.HashSet.Arbitrary ()
import System.Nix.Arbitrary.ContentAddress ()
import System.Nix.Arbitrary.Hash ()
import System.Nix.Arbitrary.Signature ()
import System.Nix.Arbitrary.StorePath ()
import System.Nix.Arbitrary.UTCTime ()
import System.Nix.StorePath (StorePath)
import System.Nix.StorePath.Metadata (Metadata(..), StorePathTrust)

import System.Nix.Hash qualified

import Test.QuickCheck (Arbitrary(..), suchThat)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))

deriving via GenericArbitrary StorePathTrust
  instance Arbitrary StorePathTrust

instance Arbitrary (Metadata StorePath) where
  arbitrary = do
    metadataDeriverPath <- arbitrary
    metadataNarHash <- (System.Nix.Hash.HashAlgo_SHA256 :=>) <$> arbitrary
    metadataReferences <- arbitrary
    metadataRegistrationTime <- arbitrary
    metadataNarBytes <- arbitrary `suchThat` (/= Just 0)
    metadataTrust <- arbitrary
    metadataSigs <- arbitrary
    metadataContentAddress <- arbitrary
    pure Metadata{..}
