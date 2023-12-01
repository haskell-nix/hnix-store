module System.Nix.Store.Remote.Types.StoreText
  ( StoreText(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.StorePath (StorePathName)

data StoreText = StoreText
  { storeTextName :: StorePathName
  , storeTextText :: Text
  } deriving (Eq, Generic, Ord, Show)
