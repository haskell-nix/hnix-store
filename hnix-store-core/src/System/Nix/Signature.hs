{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-|
Description : Nix-relevant interfaces to NaCl signatures.
-}

module System.Nix.Signature
  ( Signature(..)
  , NarSignature(..)
  , signatureParser
  , parseSignature
  , signatureToText
  ) where

import Crypto.Error (CryptoFailable(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.Base (decodeWith, encodeWith, BaseEncoding(Base64))

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Attoparsec.Text
import qualified Data.ByteArray
import qualified Data.Char
import qualified Data.Text

-- | An ed25519 signature.
newtype Signature = Signature Ed25519.Signature
  deriving (Eq, Generic, Show)

-- | A detached signature attesting to a nix archive's validity.
data NarSignature = NarSignature
  { -- | The name of the public key used to sign the archive.
    publicKey :: !Text
  , -- | The archive's signature.
    sig :: !Signature
  }
  deriving (Eq, Generic, Ord)

instance Ord Signature where
  compare (Signature x) (Signature y) = let
    xBS = Data.ByteArray.convert x :: ByteString
    yBS = Data.ByteArray.convert y :: ByteString
    in compare xBS yBS

signatureParser :: Data.Attoparsec.Text.Parser NarSignature
signatureParser = do
  publicKey <- Data.Attoparsec.Text.takeWhile1 (/= ':')
  _ <- Data.Attoparsec.Text.string ":"
  encodedSig <- Data.Attoparsec.Text.takeWhile1 (\c -> Data.Char.isAlphaNum c || c == '+' || c == '/' || c == '=')
  decodedSig <- case decodeWith Base64 encodedSig of
    Left e -> fail e
    Right decodedSig -> pure decodedSig
  sig <- case Ed25519.signature decodedSig of
    CryptoFailed e -> (fail . show) e
    CryptoPassed sig -> pure sig
  pure $ NarSignature publicKey (Signature sig)

parseSignature :: Text -> Either String NarSignature
parseSignature = Data.Attoparsec.Text.parseOnly signatureParser

signatureToText :: NarSignature -> Text
signatureToText NarSignature {publicKey, sig=Signature sig'} = let
  b64Encoded = encodeWith Base64 (Data.ByteArray.convert sig' :: ByteString)
  in mconcat [ publicKey, ":", b64Encoded ]

instance Show NarSignature where
  show narSig = Data.Text.unpack (signatureToText narSig)
