module System.Nix.Store.Remote.Types.SuccessCodeReply
  ( SuccessCodeReply(..)
  ) where

import GHC.Generics (Generic)

-- | Reply that checks an int success return value
data SuccessCodeReply = SuccessCodeReply
  deriving (Eq, Show, Generic)

