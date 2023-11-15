{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes          #-}
{-# language DataKinds           #-}

module System.Nix.Store.Remote.Parsers
  ( parseContentAddressableAddress
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import           Data.Attoparsec.ByteString.Char8
import           System.Nix.Hash
import           System.Nix.StorePath           ( ContentAddressableAddress(..)
                                                , NarHashMode(..)
                                                )
import           Crypto.Hash                    ( SHA256 )
import qualified Data.Text.Encoding

-- | Parse `ContentAddressableAddress` from `ByteString`
parseContentAddressableAddress
  :: ByteString -> Either String ContentAddressableAddress
parseContentAddressableAddress =
  Data.Attoparsec.ByteString.Char8.parseOnly contentAddressableAddressParser

-- | Parser for content addressable field
contentAddressableAddressParser :: Parser ContentAddressableAddress
contentAddressableAddressParser = caText <|> caFixed

-- | Parser for @text:sha256:<h>@
caText :: Parser ContentAddressableAddress
caText = do
  _      <- "text:sha256:"
  digest <- decodeDigestWith @SHA256 NixBase32 <$> parseHash
  either fail pure $ Text <$> digest

-- | Parser for @fixed:<r?>:<ht>:<h>@
caFixed :: Parser ContentAddressableAddress
caFixed = do
  _           <- "fixed:"
  narHashMode <- (Recursive <$ "r:") <|> (RegularFile <$ "")
  digest      <- parseTypedDigest
  either fail pure $ Fixed narHashMode <$> digest

parseTypedDigest :: Parser (Either String SomeNamedDigest)
parseTypedDigest = mkNamedDigest <$> parseHashType <*> parseHash

parseHashType :: Parser Text
parseHashType =
  Data.Text.Encoding.decodeUtf8
  <$> ("sha256" <|> "sha512" <|> "sha1" <|> "md5") <* (":" <|> "-")

parseHash :: Parser Text
parseHash =
  Data.Text.Encoding.decodeUtf8
  <$> takeWhile1 (/= ':')
