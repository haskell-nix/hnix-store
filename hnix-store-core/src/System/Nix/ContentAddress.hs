{-# LANGUAGE OverloadedStrings #-}

module System.Nix.ContentAddress (
    ContentAddress (..)
  , methodToText
  , textToMethod
  , ContentAddressMethod (..)
  , contentAddressBuilder
  , contentAddressParser
  , buildContentAddress
  , parseContentAddress
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Crypto.Hash (Digest)
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified
import Data.Dependent.Sum (DSum)
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified
import GHC.Generics (Generic)

import System.Nix.Hash (HashAlgo)
import System.Nix.Hash qualified

data ContentAddressMethod
  = ContentAddressMethod_Flat
  | ContentAddressMethod_NixArchive
  | ContentAddressMethod_Text
  -- ^ The path is a plain file added via makeTextPath or
  -- addTextToStore. It is addressed according to a sha256sum of the
  -- file contents.
  deriving (Eq, Generic, Ord, Show)

methodToText :: ContentAddressMethod -> Text
methodToText = \case
  ContentAddressMethod_Flat -> "flat"
  ContentAddressMethod_NixArchive -> "nar"
  ContentAddressMethod_Text -> "text"

textToMethod :: Text -> Either String ContentAddressMethod
textToMethod = \case
  "flat" -> Right ContentAddressMethod_Flat
  "nar" -> Right ContentAddressMethod_NixArchive
  "text" -> Right ContentAddressMethod_Text
  name -> Left $ "Unknown store object content-addressing method " <> Data.Text.unpack name

instance NFData ContentAddressMethod

-- | An address for a content-addressable store path, i.e. one whose
-- store path hash is purely a function of its contents (as opposed to
-- paths that are derivation outputs, whose hashes are a function of
-- the contents of the derivation file instead).
--
-- For backwards-compatibility reasons, the same information is
-- encodable in multiple ways, depending on the method used to add the
-- path to the store. These unfortunately result in separate store
-- paths.
data ContentAddress = ContentAddress
  ContentAddressMethod
  (DSum HashAlgo Digest)
  deriving (Eq, Generic, Ord, Show)

-- | Marshall `ContentAddressableAddress` to `Text`
-- in form suitable for remote protocol usage.
buildContentAddress :: ContentAddress -> Text
buildContentAddress =
  Data.Text.Lazy.toStrict
  . Data.Text.Lazy.Builder.toLazyText
  . contentAddressBuilder

contentAddressBuilder :: ContentAddress -> Builder
contentAddressBuilder (ContentAddress method digest) =
  (case method of
    ContentAddressMethod_Text -> "text"
    ContentAddressMethod_NixArchive -> "fixed:r"
    ContentAddressMethod_Flat -> "fixed"
  )
  <> ":"
  <> System.Nix.Hash.algoDigestBuilder digest

-- | Parse `ContentAddressableAddress` from `ByteString`
parseContentAddress
  :: Text -> Either String ContentAddress
parseContentAddress =
  Data.Attoparsec.Text.parseOnly contentAddressParser

-- | Parser for content addressable field
contentAddressParser :: Parser ContentAddress
contentAddressParser = do
  method <- parseContentAddressMethod
  _ <- ":"
  digest <- parseTypedDigest
  case digest of
    Left e -> fail e
    Right x -> return $ ContentAddress method x

parseContentAddressMethod :: Parser ContentAddressMethod
parseContentAddressMethod =
      (ContentAddressMethod_Text <$ "text")
  <|> (ContentAddressMethod_NixArchive <$ "fixed:r")
  <|> (ContentAddressMethod_Flat <$ "fixed")

parseTypedDigest :: Parser (Either String (DSum HashAlgo Digest))
parseTypedDigest = System.Nix.Hash.mkNamedDigest <$> parseHashType <*> parseHash
  where
    parseHashType :: Parser Text
    parseHashType =
      ("sha256" <|> "sha512" <|> "sha1" <|> "md5") <* (":" <|> "-")

    parseHash :: Parser Text
    parseHash = Data.Attoparsec.Text.takeWhile1 (/= ':')
