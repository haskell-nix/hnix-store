{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Store.Remote.Types.StoreConfig
  ( PreStoreConfig(..)
  , StoreConfig(..)
  , TestStoreConfig(..)
  , HasStoreSocket(..)
  ) where

import GHC.Generics (Generic)
import Network.Socket (Socket)
import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)

data PreStoreConfig = PreStoreConfig
  { preStoreConfig_dir :: StoreDir
  , preStoreConfig_socket :: Socket
  }

instance HasStoreDir PreStoreConfig where
  hasStoreDir = preStoreConfig_dir

class HasStoreSocket r where
  hasStoreSocket :: r -> Socket

instance HasStoreSocket Socket where
  hasStoreSocket = id

instance HasStoreSocket PreStoreConfig where
  hasStoreSocket = preStoreConfig_socket

data StoreConfig = StoreConfig
  { storeConfig_dir :: StoreDir
  , storeConfig_protoVersion :: ProtoVersion
  , storeConfig_socket :: Socket
  }

instance HasStoreDir StoreDir where
  hasStoreDir = id

instance HasStoreDir StoreConfig where
  hasStoreDir = storeConfig_dir

instance HasProtoVersion StoreConfig where
  hasProtoVersion = storeConfig_protoVersion

instance HasStoreSocket StoreConfig where
  hasStoreSocket = storeConfig_socket

data TestStoreConfig = TestStoreConfig
  { testStoreConfig_dir :: StoreDir
  , testStoreConfig_protoVersion :: ProtoVersion
  } deriving (Eq, Generic, Ord, Show)

instance HasProtoVersion TestStoreConfig where
  hasProtoVersion = testStoreConfig_protoVersion

instance HasStoreDir TestStoreConfig where
  hasStoreDir = testStoreConfig_dir
