{-|
Description : Derivation realisations
-}

module System.Nix.Realisation (
    BuildTraceKey(..)
  , BuildTraceKeyError(..)
  , buildTraceKeyBuilder
  , buildTraceKeyParser
  , Realisation(..)
  , RealisationWithId(..)
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import GHC.Generics (Generic)
import System.Nix.OutputName (OutputName, InvalidNameError)
import System.Nix.Signature (NamedSignature)
import System.Nix.StorePath (StorePath, InvalidPathError)

import Data.Bifunctor qualified
import Data.Text qualified
import Data.Text.Lazy.Builder qualified
import System.Nix.OutputName qualified
import System.Nix.StorePath qualified

-- | Output of the derivation
data BuildTraceKey = BuildTraceKey
  { buildTraceKeyDrvPath :: StorePath
  -- ^ Store path of the derivation
  , buildTraceKeyOutput :: OutputName
  -- ^ Output name
  } deriving (Eq, Generic, Ord, Show)

data BuildTraceKeyError
  = BuildTraceKeyError_Path InvalidPathError
  | BuildTraceKeyError_Name InvalidNameError
  | BuildTraceKeyError_NoCaret
  | BuildTraceKeyError_TooManyParts [Text]
  deriving (Eq, Ord, Show)

buildTraceKeyParser
  :: Text
  -> Either BuildTraceKeyError BuildTraceKey
buildTraceKeyParser dOut =
  case Data.Text.splitOn (Data.Text.singleton '^') dOut of
    [pathText, oName] -> do
      path <-
        Data.Bifunctor.first
          BuildTraceKeyError_Path
          $ System.Nix.StorePath.parseBasePathFromText pathText
      name <-
        Data.Bifunctor.first
          BuildTraceKeyError_Name
          $ System.Nix.OutputName.mkOutputName oName
      pure $ BuildTraceKey path name
    [_] -> Left BuildTraceKeyError_NoCaret
    x -> Left $ BuildTraceKeyError_TooManyParts x

buildTraceKeyBuilder
  :: BuildTraceKey
  -> Builder
buildTraceKeyBuilder BuildTraceKey{..} =
     Data.Text.Lazy.Builder.fromText
       (System.Nix.StorePath.storePathBaseToText buildTraceKeyDrvPath)
  <> Data.Text.Lazy.Builder.singleton '^'
  <> Data.Text.Lazy.Builder.fromText
       (System.Nix.OutputName.outputNameToText buildTraceKeyOutput)

-- | Build realisation context
--
-- realisationId is ommited since it is a key
-- of type @BuildTraceKey OutputName@ so
-- we will use @RealisationWithId@ newtype
data Realisation = Realisation
  { realisationOutPath :: StorePath
  -- ^ Output path
  , realisationSignatures :: Set NamedSignature
  -- ^ Signatures
  } deriving (Eq, Generic, Ord, Show)

-- | For wire protocol
--
-- We store this normalized in @Build.buildResultBuiltOutputs@
-- as @Map BuildTraceKey Realisation@
-- but wire protocol needs it de-normalized so we
-- need a special (From|To)JSON instances for it
newtype RealisationWithId = RealisationWithId
  { unRealisationWithId :: (BuildTraceKey, Realisation)
  }
  deriving (Eq, Generic, Ord, Show)
