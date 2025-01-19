{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shared types

module System.Nix.Derivation
    ( -- * Types
      Derivation'(..)
    , Derivation
    , BasicDerivation

    , DerivationType(..)
    , DerivationOutputs
    , InputAddressedDerivationOutput(..)
    , FixedDerivationOutput(..)
    , ContentAddressedDerivationOutput(..)

    , FreeformDerivationOutput(..)
    , FreeformDerivationOutputs
    , toSpecificOutput
    , fromSpecificOutput
    , toSpecificOutputs
    , fromSpecificOutputs

    , DerivationInputs(..)
    , derivationInputsFromSingleDerivedPath
    , derivationInputsToDerivedPaths

    , DerivedPathMap(..)
    , ChildNode(..)
    , derivedPathMapFromSingleDerivedPathBuilt
    , derivedPathMapToSet
    ) where

import Control.Monad (when)
import Control.DeepSeq (NFData(..))
import Crypto.Hash (Digest)
import Data.Constraint.Extras
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Map.Monoidal qualified as MonoidalDMap
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.GADT.DeepSeq (GNFData(..))
import Data.Kind
import Data.Map (Map)
import Data.Map qualified
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified
import Data.Set (Set)
import Data.Set qualified
import Data.Some (Some(..))
import Data.Text (Text)
import Data.These (These(..), fromThese)
import Data.Vector (Vector)
import GHC.Generics (Generic, (:.:)(..))

import System.Nix.ContentAddress (ContentAddressMethod)
import System.Nix.DerivedPath (SingleDerivedPath(..))
import System.Nix.Hash (HashAlgo)
import System.Nix.OutputName (OutputName, outputStoreObjectName)
import System.Nix.StorePath (StoreDir, StorePath, StorePathName)
import System.Nix.StorePath.ContentAddressed

-- | The type of the derivation
data DerivationType :: Type -> Type where

  -- | The outputs are input-addressed.
  DerivationType_InputAddressing :: DerivationType InputAddressedDerivationOutput

  -- | The outputs are content-addressed, and the content addresses are
  -- "fixed", i.e. required to be specific values (or the build fails)
  -- by the derivation itself.
  DerivationType_Fixed :: DerivationType FixedDerivationOutput

  -- | The outputs are content-addressed, and the content addresses are
  -- "floating", i.e. they are not required to be a specific value like
  -- in the "fixed" case.
  DerivationType_ContentAddressing :: DerivationType ContentAddressedDerivationOutput

----------------

type DerivationOutputs = DSum DerivationType (Map OutputName)

-- | An output of a Nix derivation
data InputAddressedDerivationOutput = InputAddressedDerivationOutput
        { iaPath     :: StorePath
        -- ^ Path where the output will be saved
        }
    deriving (Eq, Generic, Ord, Show)

instance NFData InputAddressedDerivationOutput

data FixedDerivationOutput = FixedDerivationOutput
        { fMethod   :: ContentAddressMethod
        -- ^ How this output is serialized into a hash / what sort of CA
        -- store path is used.
        , fHash     :: DSum HashAlgo Digest
        -- ^ Expected hash of this output
        }
    deriving (Eq, Generic, Ord, Show)

instance NFData FixedDerivationOutput

data ContentAddressedDerivationOutput = ContentAddressedDerivationOutput
        { caMethod   :: ContentAddressMethod
        -- ^ How this output is serialized into a hash / what sort of CA
        -- store path is used.
        , caHashAlgo :: Some HashAlgo
        -- ^ What sort of hash function is used with the above
        -- content-addressing method to produce the (content-addressed)
        -- store path we'll use for the output.
        }
    deriving (Eq, Generic, Ord, Show)

instance NFData ContentAddressedDerivationOutput

----------------

-- | TODO this should go in `dependent-sum`
instance (GNFData k, Has' NFData k v) => NFData (DSum k v) where
  rnf (x :=> y) = grnf x `seq` has' @NFData @v x (rnf y)

-- | TODO this needs a home
instance GNFData Digest where
  grnf = rnf

----------------

deriveGEq ''DerivationType
deriveGCompare ''DerivationType
deriveGShow ''DerivationType
deriveArgDict ''DerivationType

----------------

data Derivation' inputs outputs = Derivation
    { name      :: StorePathName
    -- ^ Name of the derivation, needed for calculating output paths
    , outputs   :: outputs
    -- ^ Outputs produced by this derivation where keys are output names
    , inputs    :: inputs
    -- ^ Inputs (sources and derivations)
    , platform  :: Text
    -- ^ Platform required for this derivation
    , builder   :: Text
    -- ^ Code to build the derivation, which can be a path or a builtin function
    , args      :: Vector Text
    -- ^ Arguments passed to the executable used to build to derivation
    , env       :: Map Text Text
    -- ^ Environment variables provided to the executable used to build the
    -- derivation
    } deriving (Eq, Generic, Ord, Show)

instance (NFData inputs, NFData output) => NFData (Derivation' inputs output)

-- | A regular Nix derivation
type Derivation = Derivation' DerivationInputs DerivationOutputs

-- | A Nix derivation that only depends on other store objects directly,
-- not (the outputs of) other derivations
type BasicDerivation = Derivation' (Set StorePath) DerivationOutputs

----------------

data DerivationInputs = DerivationInputs
    { srcs :: Set StorePath
    -- ^ Inputs that are sources
    , drvs :: DerivedPathMap
    -- ^ Inputs that are derivations where keys specify derivation paths and
    -- values specify which output names are used by this derivation
    } deriving (Eq, Generic, Ord, Show)

instance NFData DerivationInputs

instance Semigroup DerivationInputs where
  DerivationInputs x0 x1 <> DerivationInputs y0 y1 = DerivationInputs
    (x0 <> y0)
    (x1 <> y1)

instance Monoid DerivationInputs where
  mempty = DerivationInputs mempty mempty

derivationInputsFromSingleDerivedPath :: SingleDerivedPath -> DerivationInputs
derivationInputsFromSingleDerivedPath = \case
  SingleDerivedPath_Opaque storePath -> DerivationInputs
    { srcs = Data.Set.singleton storePath
    , drvs = mempty
    }
  SingleDerivedPath_Built drvDPath outputName -> DerivationInputs
    { srcs = mempty
    , drvs = derivedPathMapFromSingleDerivedPathBuilt drvDPath outputName
    }

derivationInputsToDerivedPaths :: DerivationInputs -> Set SingleDerivedPath
derivationInputsToDerivedPaths inputs =
   Data.Set.mapMonotonic SingleDerivedPath_Opaque (srcs inputs)
   <>
   derivedPathMapToSet (drvs inputs)

-- | A recursive map to handle dependencies on dynamic derivations in
-- addition to static ones
newtype DerivedPathMap = DerivedPathMap
  { unDerivedPathMap :: MonoidalMap StorePath ChildNode
  } deriving (Eq, Generic, Ord, Show)
    deriving newtype (Semigroup, Monoid)

instance NFData DerivedPathMap

newtype ChildNode = ChildNode
  { unChildNode :: These (Set OutputName) (MonoidalMap OutputName ChildNode)
  } deriving (Eq, Generic, Ord, Show)
    deriving newtype (Semigroup)

instance NFData ChildNode

derivedPathMapFromSingleDerivedPathBuilt :: SingleDerivedPath -> OutputName -> DerivedPathMap
derivedPathMapFromSingleDerivedPathBuilt drvDPath outputName0 = go drvDPath $ ChildNode $ This $ Data.Set.singleton outputName0
 where
  go :: SingleDerivedPath -> ChildNode -> DerivedPathMap
  go d child = case d of
    SingleDerivedPath_Opaque drvPath -> DerivedPathMap $ Data.Map.Monoidal.singleton drvPath child
    SingleDerivedPath_Built nestedPath nestedOutputName -> go nestedPath $ ChildNode $ That $ Data.Map.Monoidal.singleton nestedOutputName child

derivedPathMapToSet :: DerivedPathMap -> Set SingleDerivedPath
derivedPathMapToSet (DerivedPathMap m) = Data.Set.unions $ fmap
    (\(p, c) -> go (SingleDerivedPath_Opaque p) c)
    (Data.Map.Monoidal.toList m)
 where
   go :: SingleDerivedPath -> ChildNode -> Set SingleDerivedPath
   go accum (ChildNode child) =
        Data.Set.mapMonotonic (SingleDerivedPath_Built accum) shallows
        <>
        Data.Set.unions (fmap
          (\(outputName, child') -> go (SingleDerivedPath_Built accum outputName) child')
          $ Data.Map.Monoidal.toList deeps)
     where (shallows, deeps) = fromThese mempty mempty child

----------------

-- | This single data type can represent all types of derivation
-- outputs, but allows for many illegal states. This is here as a
-- simpler intermediate data type to aid with derivation parsing (both
-- JSON and ATerm).
data FreeformDerivationOutput
    = FreeformDerivationOutput
        { mPath     :: Maybe StorePath
        -- ^ Optional: Path where the output will be saved
        , mContentAddressing :: Maybe (ContentAddressMethod, DSum HashAlgo (Maybe :.: Digest))
        -- ^ Optional: How this output is serialized into a hash / what sort of CA
        -- store path is used.
        --
        -- Inner Optional: Expected hash algorithm and also possibly hash
        -- for this output.
        }
    deriving (Eq, Generic, Ord, Show)

instance NFData FreeformDerivationOutput

-- | TODO upstream
instance NFData (f (g a)) => NFData ((f :.: g) a) where
  rnf (Comp1 x) = rnf x

type FreeformDerivationOutputs = Map OutputName FreeformDerivationOutput

-- | Convert a 'FreeformDerivationOutput' to a derivation type and
-- output
toSpecificOutput
  :: forall m
  .  MonadFail m
  => StoreDir
  -> StorePathName
  -> OutputName
  -> FreeformDerivationOutput
  -> m (DSum DerivationType Identity)
toSpecificOutput storeDir drvName outputName = \case
  FreeformDerivationOutput
    { mPath = Just path
    , mContentAddressing = Nothing
    } -> pure $ DerivationType_InputAddressing :=> Identity (InputAddressedDerivationOutput path)
  FreeformDerivationOutput
    { mPath = Just path
    , mContentAddressing = Just (method, algo :=> Comp1 (Just hash))
    } -> do
      fullOutputName <- either (fail . show) pure $
        outputStoreObjectName drvName outputName
      let hash' = algo :=> hash
      let expectedPath = makeFixedOutputPath storeDir method hash' mempty fullOutputName
      when (path /= expectedPath) $
        fail "fixed output path does not match info"
      pure $ DerivationType_Fixed :=> Identity (FixedDerivationOutput method hash')
  FreeformDerivationOutput
    { mPath = Nothing
    , mContentAddressing = Just (method, algo :=> Comp1 Nothing)
    } -> pure $ DerivationType_ContentAddressing :=> Identity (ContentAddressedDerivationOutput method (Some algo))
  _ -> fail "Invalid combination of path/method/hash being present or absent"

-- | Convert a derivation type and output to a 'FreeformDerivationOutput'
fromSpecificOutput
  :: StoreDir
  -> StorePathName
  -> OutputName
  -> DSum DerivationType Identity
  -> FreeformDerivationOutput
fromSpecificOutput storeDir drvName outputName (ty :=> Identity output) = case ty of
  DerivationType_InputAddressing ->
    case output of
      InputAddressedDerivationOutput { iaPath } ->
        FreeformDerivationOutput
          { mPath = Just iaPath
          , mContentAddressing = Nothing
          }
  DerivationType_Fixed ->
    case output of
      FixedDerivationOutput { fMethod, fHash = hash'@(algo :=> hash) } ->
        FreeformDerivationOutput
          { mPath = Just $ makeFixedOutputPath storeDir fMethod hash' mempty
              $ either (error . show) id -- TODO do better
              $ outputStoreObjectName drvName outputName
          , mContentAddressing = Just (fMethod, algo :=> Comp1 (Just hash))
          }
  DerivationType_ContentAddressing ->
    case output of
      ContentAddressedDerivationOutput { caMethod, caHashAlgo = Some algo } ->
        FreeformDerivationOutput
          { mPath = Nothing
          , mContentAddressing = Just (caMethod, algo :=> Comp1 Nothing)
          }

-- | Convert a map of 'FreeformDerivationOutput' to 'DerivationOutputs'
toSpecificOutputs
  :: forall m
  .  MonadFail m
  => StoreDir
  -> StorePathName
  -> FreeformDerivationOutputs
  -> m DerivationOutputs
toSpecificOutputs storeDir drvName outputs = do
  -- Traverse and convert each output
  converted <- Data.Map.traverseWithKey (toSpecificOutput storeDir drvName) outputs
  -- Group outputs by their derivation type
  let grouped = foldMap
        (\(name, ty :=> Identity output) -> MonoidalDMap.singleton ty $ Data.Map.singleton name output)
        (Data.Map.toList converted)
  case MonoidalDMap.toList grouped of
    [res] -> pure res
    _ -> fail "derivation outputs did not agree on derivation type"

-- | Convert a map of specific derivation outputs to a 'FreeformDerivationOutputs'
fromSpecificOutputs
  :: StoreDir
  -> StorePathName
  -> DerivationOutputs
  -> FreeformDerivationOutputs
fromSpecificOutputs storeDir drvName (drvType :=> outputs) =
  flip Data.Map.mapWithKey outputs $ \outputName output ->
     fromSpecificOutput storeDir drvName outputName $ drvType :=> Identity output
