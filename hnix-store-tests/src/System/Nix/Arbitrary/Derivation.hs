{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Derivation where

import Data.Constraint.Extras
import Data.Dependent.Sum
import Data.Map
import Data.Map.Monoidal
import Data.Set
import Data.Some
import Data.Text.Arbitrary ()
import Data.Text
import Data.These
import Data.Vector.Arbitrary ()
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

import System.Nix.StorePath
import System.Nix.ContentAddress
import System.Nix.Hash
import System.Nix.Derivation
import System.Nix.OutputName
import System.Nix.Arbitrary.ContentAddress ()
import System.Nix.Arbitrary.Hash (genDSum)
import System.Nix.Arbitrary.StorePath ()
import System.Nix.Arbitrary.OutputName ()

-- | ensure output path name is not too long
shortEnoughOutputName :: StorePathName -> Gen OutputName
shortEnoughOutputName drvName =
  if availableSpace < 1 then out
  else oneof [out, resize availableSpace arbitrary]
 where
  nameLen = Data.Text.length (unStorePathName drvName)
  availableSpace = 211 - nameLen - 1 -- for the - in <drvName>-<outputname>
  out = pure $ OutputName $ either undefined id $ mkStorePathName $ pack "out"

-- | Also ensures at least one output
shortEnoughOutputsName :: Arbitrary a => StorePathName -> Gen (Map OutputName a)
shortEnoughOutputsName drvName = fmap Data.Map.fromList $ listOf1 $ (,) <$> shortEnoughOutputName drvName <*> arbitrary

shortEnoughOutputs :: StorePathName -> Gen DerivationOutputs
shortEnoughOutputs drvName =
  genDSum arbitrary $ \tag -> has @Arbitrary tag $ shortEnoughOutputsName drvName

-- | Ensure a valid combination
ensureValidMethodAlgo :: ContentAddressMethod -> HashAlgo a -> Bool
ensureValidMethodAlgo ContentAddressMethod_Text HashAlgo_SHA256 = True
ensureValidMethodAlgo ContentAddressMethod_Text _ = False
ensureValidMethodAlgo _ _ = True

instance
  ( Arbitrary inputs
  , Arbitrary output
  , Arg (Derivation' inputs (Map OutputName output)) inputs
  , Arg (Derivation' inputs (Map OutputName output)) output
  ) => Arbitrary (Derivation' inputs (Map OutputName output))
 where
  arbitrary = do
    drv <- genericArbitrary
    om <- shortEnoughOutputsName $ name drv
    let
      drv' = drv { outputs = om }
      -- type inference hint
      _ = [drv, drv']
    pure drv'
  shrink = genericShrink

instance
  ( Arbitrary inputs
  , Arg (Derivation' inputs DerivationOutputs) inputs
  ) => Arbitrary (Derivation' inputs DerivationOutputs)
 where
  arbitrary = do
    drv <- genericArbitrary
    os <- shortEnoughOutputs $ name drv
    let
      drv' = drv { outputs = os }
      -- type inference hint
      _ = [drv, drv']
    pure drv'
  shrink = genericShrink

deriving via GenericArbitrary FreeformDerivationOutput
  instance Arbitrary FreeformDerivationOutput

deriving via GenericArbitrary InputAddressedDerivationOutput
  instance Arbitrary InputAddressedDerivationOutput

instance Arbitrary FixedDerivationOutput where
  arbitrary = genericArbitrary `suchThat`
    \(FixedDerivationOutput {fMethod, fHash = hashAlgo :=> _}) ->
      ensureValidMethodAlgo fMethod hashAlgo

instance Arbitrary ContentAddressedDerivationOutput where
  arbitrary = genericArbitrary `suchThat`
    \(ContentAddressedDerivationOutput {caMethod, caHashAlgo = Some hashAlgo }) ->
      ensureValidMethodAlgo caMethod hashAlgo

instance Arbitrary (Some DerivationType)  where
  arbitrary =
    oneof
    $ pure
    <$> [
      Some DerivationType_InputAddressing
    , Some DerivationType_Fixed
    , Some DerivationType_ContentAddressing
    ]

deriving via GenericArbitrary DerivationInputs
  instance Arbitrary DerivationInputs

deriving via GenericArbitrary DerivedPathMap
  instance Arbitrary DerivedPathMap

instance Arbitrary ChildNode where
  -- Scale down exponentially, or the resulting tree may explode in size.
  arbitrary = scale (`div` 2) $ do
   oneof [
      ChildNode . This . Data.Set.fromList <$> ((:) <$> arbitrary <*> arbitrary)
    , ChildNode . That . Data.Map.Monoidal.fromList <$> ((:) <$> arbitrary <*> arbitrary)
    ]

-- TODO these belong elsewhere

deriving newtype instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (MonoidalMap k v)

deriving via GenericArbitrary (These a b)
  instance ( Arg (These a b) a
           , Arg (These a b) b
           , Arbitrary a
           , Arbitrary b
           ) => Arbitrary (These a b)
