{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}

module System.Nix.Store.Remote.Parsers
  ( parseContentAddressableAddress
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8
import qualified Data.Text
import           Data.Text.Encoding             ( decodeUtf8 )
import           System.Nix.Hash
import           System.Nix.StorePath           ( ContentAddressableAddress(..)
                                                , NarHashMode(..)
                                                )

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
  digest <- decodeBase32 @'SHA256 <$> parseHash
  either fail return $ Text <$> digest

-- | Parser for @fixed:<r?>:<ht>:<h>@
caFixed :: Parser ContentAddressableAddress
caFixed = do
  _           <- "fixed:"
  narHashMode <- (pure Recursive <$> "r:") <|> (pure RegularFile <$> "")
  digest      <- parseTypedDigest
  either fail return $ Fixed narHashMode <$> digest

parseTypedDigest :: Parser (Either String SomeNamedDigest)
parseTypedDigest = mkNamedDigest <$> parseHashType <*> parseHash

parseHashType :: Parser Data.Text.Text
parseHashType = decodeUtf8 <$> ("sha256" <|> "sha512" <|> "sha1" <|> "md5") <* (":" <|> "-")

parseHash :: Parser Data.Text.Text
parseHash = decodeUtf8 <$> takeWhile1 (/= ':')
