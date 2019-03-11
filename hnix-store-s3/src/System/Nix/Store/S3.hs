module System.Nix.Store.S3 (
  s3BinaryStoreEffects
) where

import Control.Lens
import System.Nix.Store.Binary
import Control.Monad (void)
import Network.AWS
import Network.AWS.S3

s3BinaryStoreEffects
  :: (MonadAWS m)
  => BucketName
  -> BinaryStoreEffects m
s3BinaryStoreEffects bucketName' =
  BinaryStoreEffects upsertFile'
  where
    upsertFile' pth d mimeType =
      void . send $ putObject bucketName' (ObjectKey pth) (toBody d) & poContentType ?~ mimeType
