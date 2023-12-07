module System.Nix.Store.Remote.Types.Handshake
  ( ClientHandshakeInput(..)
  , ClientHandshakeOutput(..)
  , ServerHandshakeInput(..)
  , ServerHandshakeOutput(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion)
import System.Nix.Store.Remote.Types.TrustedFlag (TrustedFlag)

-- | Data sent by the client during initial protocol handshake
data ClientHandshakeInput = ClientHandshakeInput
  { clientHandshakeInputOurVersion :: ProtoVersion -- ^ Our protocol version (that we advertise to the server)
  } deriving (Eq, Generic, Ord, Show)

-- | Data received by the client via initial protocol handshake
data ClientHandshakeOutput = ClientHandshakeOutput
  { clientHandshakeOutputNixVersion :: Maybe Text -- ^ Textual version, since 1.33
  , clientHandshakeOutputTrust :: Maybe TrustedFlag -- ^ Whether remote side trusts us
  , clientHandshakeOutputLeastCommonVerison :: ProtoVersion -- ^ Minimum protocol version supported by both sides
  , clientHandshakeOutputServerVersion :: ProtoVersion -- ^ Protocol version supported by the server
  } deriving (Eq, Generic, Ord, Show)

-- | Data sent by the server during initial protocol handshake
data ServerHandshakeInput = ServerHandshakeInput
  { serverHandshakeInputNixVersion :: Text -- ^ Textual version, since 1.33
  , serverHandshakeInputOurVersion :: ProtoVersion -- ^ Our protocol version (that we advertise to the client)
  , serverHandshakeInputTrust :: Maybe TrustedFlag -- ^ Whether client should trusts us
  } deriving (Eq, Generic, Ord, Show)

-- | Data received by the server during initial protocol handshake
data ServerHandshakeOutput = ServerHandshakeOutput
  { serverHandshakeOutputLeastCommonVersion :: ProtoVersion -- ^ Minimum protocol version supported by both sides
  , serverHandshakeOutputClientVersion :: ProtoVersion -- ^ Protocol version supported by the client
  } deriving (Eq, Generic, Ord, Show)
