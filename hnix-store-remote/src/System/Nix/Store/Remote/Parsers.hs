{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}

module System.Nix.Store.Remote.Parsers (
    parseContentAddressableAddress
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, (<?>))
import Data.ByteString (ByteString)
import System.Nix.Hash (Digest, NamedAlgo, SomeNamedDigest(SomeDigest))
import System.Nix.StorePath (ContentAddressableAddress(..), NarHashMode(..))

import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8
import qualified Data.Text

import qualified System.Nix.Internal.Base32
import qualified System.Nix.Hash

-- | Parse `ContentAddressableAddress` from `ByteString`
parseContentAddressableAddress :: forall hashAlgo . NamedAlgo hashAlgo
                              => ByteString
                              -> Either String ContentAddressableAddress
parseContentAddressableAddress =
  Data.Attoparsec.ByteString.Char8.parseOnly
    (contentAddressableAddressParser @hashAlgo)

-- | Parser for content addressable field
contentAddressableAddressParser :: forall hashAlgo . NamedAlgo hashAlgo
                                => Parser ContentAddressableAddress
contentAddressableAddressParser =
      caText
  <|> caFixed @hashAlgo

-- | Parser for @text:sha256:<h>@
caText :: Parser ContentAddressableAddress
caText = do
  _ <- "text:sha256:"
  digest <- parseDigest
  either fail return
    $ Text <$> digest

-- | Parser for @fixed:<r?>:<ht>:<h>@
caFixed :: forall hashAlgo . NamedAlgo hashAlgo => Parser ContentAddressableAddress
caFixed = do
  _ <- "fixed:"
  narHashMode <- (pure Recursive <$> "true") <|> (pure RegularFile <$> "false")
    <?> "Invalid Base32 part"
  digest <- parseDigest

  either fail return
    $ Fixed <$> pure narHashMode <*> (SomeDigest @hashAlgo <$> digest)

parseDigest :: forall a . Parser (Either String (Digest a))
parseDigest =
    System.Nix.Hash.decodeBase32
  . Data.Text.pack
  . Data.ByteString.Char8.unpack
    <$> Data.Attoparsec.ByteString.Char8.takeWhile1
          (\c -> c `elem` System.Nix.Internal.Base32.digits32)
