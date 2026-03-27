{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PathInfoSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import Data.HashSet qualified
import Data.Set qualified
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Crypto.Hash (Digest)
import Data.Dependent.Sum (DSum)

import System.Nix.ContentAddress (ContentAddress(..), ContentAddressMethod(..))
import System.Nix.Hash (HashAlgo, mkNamedDigest)
import System.Nix.JSON ()
import System.Nix.Signature (NamedSignature(..))
import System.Nix.Signature qualified
import System.Nix.StorePath (StorePath)
import System.Nix.StorePath qualified
import System.Nix.StorePath.Metadata (Metadata(..), StorePathTrust(..))

barPath :: StorePath
barPath = forceRight $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar"

barDrvPath :: StorePath
barDrvPath = forceRight $ System.Nix.StorePath.parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar.drv"

fooPath :: StorePath
fooPath = forceRight $ System.Nix.StorePath.parseBasePathFromText "n5wkd9frr45pa74if5gpz9j7mifg27fh-foo"

sig1 :: NamedSignature
sig1 = NamedSignature
  { publicKey = "asdf"
  , sig = forceRight $ System.Nix.Signature.parseSignature "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=="
  }

sig2 :: NamedSignature
sig2 = NamedSignature
  { publicKey = "qwer"
  , sig = forceRight $ System.Nix.Signature.parseSignature "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=="
  }

theNarHash :: DSum HashAlgo Digest
theNarHash = forceRight $ mkNamedDigest "sha256" "FePFYIlMuycIXPZbWi7LGEiMmZSX9FMbaQenWBzm1Sc="

theCa :: ContentAddress
theCa = ContentAddress
  ContentAddressMethod_NixArchive
  (forceRight $ mkNamedDigest "sha256" "EMIJ+giQ/gLIWoxmPKjno3zHZrxbGymgzGGyZvZBIdM=")

spec :: Spec
spec = do
  let dir = "upstream-libstore-data/path-info/json-3"

  describe "upstream Nix test data (path-info json-3)" $ do
    parsesUpstream dir "pure.json" Metadata
      { metadataDeriverPath = Nothing
      , metadataNarHash = theNarHash
      , metadataReferences = Data.HashSet.fromList [barPath, fooPath]
      , metadataRegistrationTime = posixSecondsToUTCTime 0
      , metadataNarBytes = Just 34878
      , metadataTrust = BuiltElsewhere
      , metadataSigs = mempty
      , metadataContentAddress = Just theCa
      }

    parsesUpstream dir "impure.json" Metadata
      { metadataDeriverPath = Just barDrvPath
      , metadataNarHash = theNarHash
      , metadataReferences = Data.HashSet.fromList [barPath, fooPath]
      , metadataRegistrationTime = posixSecondsToUTCTime 23423
      , metadataNarBytes = Just 34878
      , metadataTrust = BuiltLocally
      , metadataSigs = Data.Set.fromList [sig1, sig2]
      , metadataContentAddress = Just theCa
      }

    parsesUpstream dir "empty_pure.json" emptyMetadata

    parsesUpstream dir "empty_impure.json" emptyMetadata

emptyMetadata :: Metadata StorePath
emptyMetadata = Metadata
  { metadataDeriverPath = Nothing
  , metadataNarHash = theNarHash
  , metadataReferences = mempty
  , metadataRegistrationTime = posixSecondsToUTCTime 0
  , metadataNarBytes = Nothing
  , metadataTrust = BuiltElsewhere
  , metadataSigs = mempty
  , metadataContentAddress = Nothing
  }
