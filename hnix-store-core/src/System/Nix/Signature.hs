{-# LANGUAGE OverloadedStrings #-}
{-|
Description : Nix-relevant interfaces to NaCl signatures.
-}

module System.Nix.Signature
  ( Signature(..)
  , NarSignature(..)
  , signatureParser
  , parseSignature
  ) where

import GHC.Generics (Generic)

import qualified Crypto.PubKey.Ed25519
import Crypto.Error (CryptoFailable(..))
import qualified Data.Attoparsec.Text
import qualified Data.Char
import qualified Data.ByteArray
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified System.Nix.Base
import System.Nix.Base (BaseEncoding(Base64))

-- | An ed25519 signature.
newtype Signature = Signature Crypto.PubKey.Ed25519.Signature
  deriving (Eq, Generic, Show)

-- | A detached signature attesting to a nix archive's validity.
data NarSignature = NarSignature
  { -- | The name of the public key used to sign the archive.
    publicKey :: !Text
  , -- | The archive's signature.
    sig :: !Signature
  }
  deriving (Eq, Generic, Ord, Show)

instance Ord Signature where
  compare (Signature x) (Signature y) = let
    xBS = Data.ByteArray.convert x :: BS.ByteString
    yBS = Data.ByteArray.convert y :: BS.ByteString
    in compare xBS yBS

signatureParser :: Data.Attoparsec.Text.Parser NarSignature
signatureParser = do
  publicKey <- Data.Attoparsec.Text.takeWhile1 (/= ':')
  _ <- Data.Attoparsec.Text.string ":"
  encodedSig <- Data.Attoparsec.Text.takeWhile1 (\c -> Data.Char.isAlphaNum c || c == '+' || c == '/' || c == '=')
  decodedSig <- case System.Nix.Base.decodeWith Base64 encodedSig of
    Left e -> fail e
    Right decodedSig -> pure decodedSig
  sig <- case Crypto.PubKey.Ed25519.signature decodedSig of
    CryptoFailed e -> (fail . show) e
    CryptoPassed sig -> pure sig
  pure $ NarSignature publicKey (Signature sig)

parseSignature
  :: Text -> Either String NarSignature
parseSignature =
  Data.Attoparsec.Text.parseOnly signatureParser

