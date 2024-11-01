{-# LANGUAGE DeriveAnyClass #-}
{-|
Description : Derived path output names
-}

module System.Nix.OutputName
  ( OutputName(..)
  , mkOutputName
  , outputNameParser
  -- * Re-exports
  , System.Nix.StorePath.InvalidNameError(..)
  , System.Nix.StorePath.parseNameText
  ) where

import Data.Attoparsec.Text.Lazy (Parser, (<?>))
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.StorePath (InvalidNameError)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text
import qualified System.Nix.StorePath

-- | Name of the derived path output
-- Typically used for "dev", "doc" sub-outputs
newtype OutputName = OutputName
  { -- | Extract the contents of the name.
    unOutputName :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

mkOutputName :: Text -> Either InvalidNameError OutputName
mkOutputName = fmap OutputName . System.Nix.StorePath.parseNameText

outputNameParser :: Parser OutputName
outputNameParser = do
  c0 <-
    Data.Attoparsec.Text.Lazy.satisfy
      (\c -> c /= '.' && System.Nix.StorePath.validStorePathNameChar c)
      <?> "Leading path name character is a dot or invalid character"

  rest <-
    Data.Attoparsec.Text.Lazy.takeWhile
    System.Nix.StorePath.validStorePathNameChar
      <?> "Path name contains invalid character"

  pure
    $ OutputName
    $ Data.Text.cons c0 rest
