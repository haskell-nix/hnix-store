{-|
Description : Derivation realisations
-}

module System.Nix.Realisation (
    DerivationOutput(..)
  , DerivationOutputError(..)
  , derivationOutputBuilder
  , derivationOutputParser
  , Realisation(..)
  , RealisationWithId(..)
  ) where

import Crypto.Hash (Digest)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Dependent.Sum (DSum)
import GHC.Generics (Generic)
import System.Nix.Hash (HashAlgo)
import System.Nix.OutputName (OutputName, InvalidNameError)
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StorePath)

import Data.Bifunctor qualified
import Data.Text qualified
import Data.Text.Lazy.Builder qualified
import System.Nix.Hash qualified

-- | Output of the derivation
data DerivationOutput a = DerivationOutput
  { derivationOutputHash :: DSum HashAlgo Digest
  -- ^ Hash modulo of the derivation
  , derivationOutputOutput :: a
  -- ^ Output (either a OutputName or StorePatH)
  } deriving (Eq, Generic, Ord, Show)

data DerivationOutputError
  = DerivationOutputError_Digest String
  | DerivationOutputError_Name InvalidNameError
  | DerivationOutputError_NoExclamationMark
  | DerivationOutputError_NoColon
  | DerivationOutputError_TooManyParts [Text]
  deriving (Eq, Ord, Show)

derivationOutputParser
  :: (Text -> Either InvalidNameError outputName)
  -> Text
  -> Either DerivationOutputError (DerivationOutput outputName)
derivationOutputParser outputName dOut =
  case Data.Text.splitOn (Data.Text.singleton '!') dOut of
    [] -> Left DerivationOutputError_NoColon
    [sriHash, oName] -> do
      hash <-
        case Data.Text.splitOn (Data.Text.singleton ':') sriHash of
          [] -> Left DerivationOutputError_NoColon
          [hashName, digest] ->
            Data.Bifunctor.first
              DerivationOutputError_Digest
              $ System.Nix.Hash.mkNamedDigest hashName digest
          x -> Left $ DerivationOutputError_TooManyParts x
      name <-
        Data.Bifunctor.first
          DerivationOutputError_Name
          $ outputName oName

      pure $ DerivationOutput hash name
    x -> Left $ DerivationOutputError_TooManyParts x

derivationOutputBuilder
  :: (outputName -> Text)
  -> DerivationOutput outputName
  -> Builder
derivationOutputBuilder outputName DerivationOutput{..} =
     System.Nix.Hash.algoDigestBuilder derivationOutputHash
  <> Data.Text.Lazy.Builder.singleton '!'
  <> Data.Text.Lazy.Builder.fromText (outputName derivationOutputOutput)

-- | Build realisation context
--
-- realisationId is ommited since it is a key
-- of type @DerivationOutput OutputName@ so
-- we will use @RealisationWithId@ newtype
data Realisation = Realisation
  { realisationOutPath :: StorePath
  -- ^ Output path
  , realisationSignatures :: Set Signature
  -- ^ Signatures
  , realisationDependencies :: Map (DerivationOutput OutputName) StorePath
  -- ^ Dependent realisations required for this one to be valid
  } deriving (Eq, Generic, Ord, Show)

-- | For wire protocol
--
-- We store this normalized in @Build.buildResultBuiltOutputs@
-- as @Map (DerivationOutput OutputName) Realisation@
-- but wire protocol needs it de-normalized so we
-- need a special (From|To)JSON instances for it
newtype RealisationWithId = RealisationWithId
  { unRealisationWithId :: (DerivationOutput OutputName, Realisation)
  }
  deriving (Eq, Generic, Ord, Show)
