{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Nix.Store.Remote.Types.StoreConfig
  ( ProtoStoreConfig(..)
  , StoreSocketPath(..)
  , StoreTCP(..)
  , StoreConnection(..)
  , HasStoreSocket(..)
  ) where

import Data.Default.Class (Default(def))
import Data.String (IsString)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)

class HasStoreSocket r where
  hasStoreSocket :: r -> Socket

instance HasStoreSocket Socket where
  hasStoreSocket = id

data ProtoStoreConfig = ProtoStoreConfig
  { protoStoreConfigDir :: StoreDir
  , protoStoreConfigProtoVersion :: ProtoVersion
  } deriving (Eq, Generic, Ord, Show)

instance Default ProtoStoreConfig where
  def = ProtoStoreConfig def def

instance HasStoreDir StoreDir where
  hasStoreDir = id

instance HasStoreDir ProtoStoreConfig where
  hasStoreDir = protoStoreConfigDir

instance HasProtoVersion ProtoStoreConfig where
  hasProtoVersion = protoStoreConfigProtoVersion

newtype StoreSocketPath = StoreSocketPath
  { unStoreSocketPath :: FilePath
  }
  deriving newtype (IsString)
  deriving stock (Eq, Generic, Ord, Show)

instance Default StoreSocketPath where
  def = StoreSocketPath "/nix/var/nix/daemon-socket/socket"

data StoreTCP = StoreTCP
  { storeTCPHost :: String
  , storeTCPPort :: Int
  } deriving (Eq, Generic, Ord, Show)

data StoreConnection
  = StoreConnection_Socket StoreSocketPath
  | StoreConnection_TCP StoreTCP
  deriving (Eq, Generic, Ord, Show)

instance Default StoreConnection where
  def = StoreConnection_Socket def
