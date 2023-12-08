{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Store.Remote.Types.StoreConfig
  ( ProtoStoreConfig(..)
  , StoreConfig(..)
  , TestStoreConfig(..)
  , HasStoreSocket(..)
  ) where

import Data.Default.Class (Default(def))
import GHC.Generics (Generic)
import Network.Socket (Socket)
import System.Nix.StorePath (HasStoreDir(..), StoreDir)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)

class HasStoreSocket r where
  hasStoreSocket :: r -> Socket

instance HasStoreSocket Socket where
  hasStoreSocket = id

data ProtoStoreConfig = ProtoStoreConfig
  { protoStoreConfig_dir :: StoreDir
  , protoStoreConfig_protoVersion :: ProtoVersion
  }

instance Default ProtoStoreConfig where
  def = ProtoStoreConfig def def

instance HasStoreDir StoreDir where
  hasStoreDir = id

instance HasStoreDir ProtoStoreConfig where
  hasStoreDir = protoStoreConfig_dir

instance HasProtoVersion ProtoStoreConfig where
  hasProtoVersion = protoStoreConfig_protoVersion

data StoreConfig = StoreConfig
  { storeConfig_dir :: StoreDir
  , storeConfig_socketPath :: FilePath
  }

-- TODO: del
data TestStoreConfig = TestStoreConfig
  { testStoreConfig_dir :: StoreDir
  , testStoreConfig_protoVersion :: ProtoVersion
  } deriving (Eq, Generic, Ord, Show)

instance HasProtoVersion TestStoreConfig where
  hasProtoVersion = testStoreConfig_protoVersion

instance HasStoreDir TestStoreConfig where
  hasStoreDir = testStoreConfig_dir
