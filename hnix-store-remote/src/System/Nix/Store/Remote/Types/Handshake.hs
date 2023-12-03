module System.Nix.Store.Remote.Types.Handshake
  ( Handshake(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion)
import System.Nix.Store.Remote.Types.TrustedFlag (TrustedFlag)

-- | Data for initial protocol handshake
data Handshake = Handshake
  { handshakeNixVersion :: Maybe Text -- ^ Textual version, since 1.33
  , handshakeTrust :: Maybe TrustedFlag -- ^ Whether remote side trusts us
  , handshakeProtoVersion :: ProtoVersion -- ^ Minimum protocol supported by both sides
  , handshakeRemoteProtoVersion :: ProtoVersion -- ^ Protocol supported by remote side
  }
  deriving (Eq, Generic, Ord, Show)
