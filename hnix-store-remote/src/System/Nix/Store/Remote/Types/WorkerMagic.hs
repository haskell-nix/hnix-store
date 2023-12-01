module System.Nix.Store.Remote.Types.WorkerMagic
  ( WorkerMagic(..)
  , workerMagicToWord64
  , word64ToWorkerMagic
  ) where

import Data.Word (Word64)
import GHC.Generics (Generic)

-- | WorkerMagic
--
-- Magic numbers exchange during handshake
data WorkerMagic
  = WorkerMagic_One
  | WorkerMagic_Two
  deriving (Eq, Generic, Ord, Show)

workerMagicToWord64 :: WorkerMagic -> Word64
workerMagicToWord64 = \case
  WorkerMagic_One -> 0x6e697863
  WorkerMagic_Two -> 0x6478696f

word64ToWorkerMagic :: Word64 -> Either String WorkerMagic
word64ToWorkerMagic = \case
  0x6e697863 -> Right WorkerMagic_One
  0x6478696f -> Right WorkerMagic_Two
  x -> Left $ "Invalid WorkerMagic: " ++ show x
