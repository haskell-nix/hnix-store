{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Store.Remote.Types.StoreConfig
  ( ProtoStoreConfig(..)
  , StoreConfig(..)
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

data StoreConfig = StoreConfig
  { storeConfigDir :: Maybe StoreDir
  , storeConfigSocketPath :: FilePath
  } deriving (Eq, Generic, Ord, Show)
