module System.Nix.Store.Remote.Types.StoreConfig
  ( PreStoreConfig(..)
  , StoreConfig(..)
  , HasStoreSocket(..)
  ) where

import Network.Socket (Socket)
import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)

data PreStoreConfig = PreStoreConfig
  { preStoreConfig_dir    :: StoreDir
  , preStoreConfig_socket :: Socket
  }

instance HasStoreDir PreStoreConfig where
  hasStoreDir = preStoreConfig_dir

class HasStoreSocket r where
  storeSocket :: r -> Socket

instance HasStoreSocket Socket where
  storeSocket = id

instance HasStoreSocket PreStoreConfig where
  storeSocket = preStoreConfig_socket

data StoreConfig = StoreConfig
  { storeConfig_dir         :: StoreDir
  , storeConfig_protoVersion :: ProtoVersion
  , storeConfig_socket      :: Socket
  }

instance HasStoreDir StoreConfig where
  hasStoreDir = storeConfig_dir

instance HasProtoVersion StoreConfig where
  hasProtoVersion = storeConfig_protoVersion

instance HasStoreSocket StoreConfig where
  storeSocket = storeConfig_socket
