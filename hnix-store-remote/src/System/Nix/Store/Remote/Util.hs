module System.Nix.Store.Remote.Util where

import           Control.Monad.Reader

import           Data.Maybe
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.HashSet              as HashSet

import           Network.Socket.ByteString (recv, sendAll)

import           System.Nix.Store.Remote.Types
import           System.Nix.Path
import           System.Nix.Util


genericIncremental :: (MonadIO m) => m (Maybe B.ByteString) -> Get a -> m a
genericIncremental getsome parser = go decoder
  where decoder = runGetIncremental parser
        go (Done _leftover _consumed x) = do
          return x
        go (Partial k) = do
          chunk <- getsome
          go (k chunk)
        go (Fail _leftover _consumed msg) = do
          error msg

getSocketIncremental :: Get a -> MonadStore a
getSocketIncremental = genericIncremental sockGet

sockPut :: Put -> MonadStore ()
sockPut p = do
  soc <- ask
  liftIO $ sendAll soc $ LBS.toStrict $ runPut p

sockGet :: MonadStore (Maybe BSC.ByteString)
sockGet = do
  soc <- ask
  liftIO $ Just <$> recv soc 8

sockGetPath :: MonadStore (Maybe Path)
sockGetPath = getSocketIncremental getPath

sockGetPaths :: MonadStore PathSet
sockGetPaths = getSocketIncremental getPaths

sockGetInt :: Integral a => MonadStore a
sockGetInt = getSocketIncremental getInt

sockGetBool :: MonadStore Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: MonadStore LBS.ByteString
sockGetStr = getSocketIncremental getByteStringLen

sockGetStrings :: MonadStore [LBS.ByteString]
sockGetStrings = getSocketIncremental getByteStrings

lBSToText :: LBS.ByteString -> Text
lBSToText = T.pack . BSC.unpack . LBS.toStrict

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . BSC.pack . T.unpack

-- XXX: needs work
mkPath :: LBS.ByteString -> Maybe Path
mkPath p = case (pathName $ lBSToText p) of
             -- TODO: replace `undefined` with digest encoding function when
             --       [issue 24](https://github.com/haskell-nix/hnix-store/issues/24)
             --       is closed
             Just x -> Just $ Path (undefined $ LBS.toStrict p) x --XXX: hash
             Nothing -> Nothing

-- WOOT
-- import           Data.ByteString.Base32    as Base32
--drvP = Path (fromJust $ digestFromByteString $ pls $ Base32.decode $ BSC.take 32 $ BSC.drop (BSC.length "/nix/store/") drv) (fromJust $ pathName $ T.pack $ BSC.unpack drv)
--pls (Left _) = error "unable to decode hash"
--pls (Right x) = x

getPath :: Get (Maybe Path)
getPath = mkPath <$> getByteStringLen

getPaths :: Get PathSet
getPaths = HashSet.fromList . catMaybes . map mkPath <$> getByteStrings

putPathName :: PathName -> Put
putPathName = putByteStringLen . textToLBS . pathNameContents

putPath :: Path -> Put
putPath (Path _hash name) = putPathName name

putPaths :: PathSet -> Put
putPaths = putByteStrings . HashSet.map (\(Path _hash name) -> textToLBS $ pathNameContents name)

putBool :: Bool -> Put
putBool True  = putInt (1 :: Int)
putBool False = putInt (0 :: Int)
