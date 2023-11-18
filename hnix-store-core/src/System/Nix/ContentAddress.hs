{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Nix.ContentAddress (
    ContentAddress
  , contentAddressBuilder
  , contentAddressParser
  ) where

import Control.Applicative
import Crypto.Hash (Digest, SHA256)
import Data.Hashable (Hashable)
import Data.Text.Lazy.Builder (Builder)
import GHC.Generics (Generic)
import System.Nix.Base (BaseEncoding(NixBase32))
import System.Nix.Hash (SomeNamedDigest(SomeDigest))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import Test.QuickCheck.Instances ()

import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Text.Encoding
import qualified System.Nix.Hash

-- | An address for a content-addressable store path, i.e. one whose
-- store path hash is purely a function of its contents (as opposed to
-- paths that are derivation outputs, whose hashes are a function of
-- the contents of the derivation file instead).
--
-- For backwards-compatibility reasons, the same information is
-- encodable in multiple ways, depending on the method used to add the
-- path to the store. These unfortunately result in separate store
-- paths.
data ContentAddress
  = -- | The path is a plain file added via makeTextPath or
    -- addTextToStore. It is addressed according to a sha256sum of the
    -- file contents.
    Text !(Digest SHA256)
  | -- | The path was added to the store via makeFixedOutputPath or
    -- addToStore. It is addressed according to some hash algorithm
    -- applied to the nar serialization via some 'NarHashMode'.
    Fixed !NarHashMode !SomeNamedDigest
  deriving (Eq, Generic, Ord, Show)

deriving via GenericArbitrary ContentAddress
  instance Arbitrary ContentAddress

-- | Builder for `ContentAddress`
contentAddressBuilder :: ContentAddress -> Builder
contentAddressBuilder (Text digest) =
  "text:"
  <> System.Nix.Hash.digestBuilder digest
contentAddressBuilder (Fixed narHashMode (SomeDigest (digest :: Digest hashAlgo))) =
  "fixed:"
  <> (if narHashMode == Recursive then "r:" else mempty)
--  <> Data.Text.Lazy.Builder.fromText (System.Nix.Hash.algoName @hashAlgo)
  <> System.Nix.Hash.digestBuilder digest

-- | Parser for content addressable field
contentAddressParser :: Data.Attoparsec.ByteString.Char8.Parser ContentAddress
contentAddressParser = caText <|> caFixed
  where
  -- | Parser for @text:sha256:<h>@
  --caText :: Parser ContentAddress
  caText = do
    _      <- "text:sha256:"
    digest <- System.Nix.Hash.decodeDigestWith @SHA256 NixBase32 <$> parseHash
    either fail pure $ Text <$> digest

  -- | Parser for @fixed:<r?>:<ht>:<h>@
  --caFixed :: Parser ContentAddress
  caFixed = do
    _           <- "fixed:"
    narHashMode <- (Recursive <$ "r:") <|> (RegularFile <$ "")
    digest      <- parseTypedDigest
    either fail pure $ Fixed narHashMode <$> digest

  --parseTypedDigest :: Parser (Either String SomeNamedDigest)
  parseTypedDigest = System.Nix.Hash.mkNamedDigest <$> parseHashType <*> parseHash

  --parseHashType :: Parser Text
  parseHashType =
    Data.Text.Encoding.decodeUtf8
    <$> ("sha256" <|> "sha512" <|> "sha1" <|> "md5") <* (":" <|> "-")

  --parseHash :: Parser Text
  parseHash =
    Data.Text.Encoding.decodeUtf8
    <$> Data.Attoparsec.ByteString.Char8.takeWhile1 (/= ':')

-- | Schemes for hashing a Nix archive.
--
-- For backwards-compatibility reasons, there are two different modes
-- here, even though 'Recursive' should be able to cover both.
data NarHashMode
  = -- | Require the nar to represent a non-executable regular file.
    RegularFile
  | -- | Hash an arbitrary nar, including a non-executable regular
    -- file if so desired.
    Recursive
  deriving (Eq, Enum, Generic, Hashable, Ord, Show)

deriving via GenericArbitrary NarHashMode
  instance Arbitrary NarHashMode
