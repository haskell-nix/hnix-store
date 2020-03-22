-- | Stream out a NAR file from a regular file

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module System.Nix.Internal.Nar.Streamer where

import           Control.Monad                   (forM, forM_, when)
import qualified Control.Monad.IO.Class          as IO
import           Data.Bool                       (bool)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as BSC
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.List                       as List
import qualified Data.Serialize                  as Serial
import           GHC.Int                         (Int64)
import qualified System.Directory                as Directory
import           System.FilePath                 ((</>))

import qualified System.Nix.Internal.Nar.Effects as Nar


-- | This implementation of Nar encoding takes an arbitrary @yield@
--   function from any streaming library, and repeatedly calls
--   it while traversing the filesystem object to Nar encode
streamNarIO
  :: forall m.(IO.MonadIO m)
  => (BS.ByteString -> m ())
  -> Nar.NarEffects IO
  -> FilePath
  -> m ()
streamNarIO yield effs basePath = do
  yield (str "nix-archive-1")
  parens (go basePath)
  where

    go :: FilePath -> m ()
    go path = do
      isDir     <- IO.liftIO $ Nar.narIsDir effs path
      isSymLink <- IO.liftIO $ Nar.narIsSymLink effs path
      let isRegular = not (isDir || isSymLink)

      when isSymLink $ do
        target <- IO.liftIO $ Nar.narReadLink effs path
        yield $
          strs ["type", "symlink", "target", BSC.pack target]

      when isRegular $ do
        isExec <- IO.liftIO $ isExecutable effs path
        yield $ strs ["type","regular"]
        when (isExec == Executable) (yield $ strs ["executable", ""])
        fSize <- IO.liftIO $ Nar.narFileSize effs path
        yield $ str "contents"
        yield $ int fSize
        yieldFile path fSize

      when isDir $ do
        fs <- IO.liftIO (Nar.narListDir effs path)
        yield $ strs ["type", "directory"]
        forM_ (List.sort fs) $ \f -> do
          yield $ str "entry"
          parens $ do
            let fullName = path </> f
            yield (strs ["name", BSC.pack f, "node"])
            parens (go fullName)

    str :: BS.ByteString -> BS.ByteString
    str t = let len =  BS.length t
            in  int len <> padBS len t

    padBS :: Int -> BS.ByteString -> BS.ByteString
    padBS strSize bs = bs <> BS.replicate (padLen strSize) 0

    parens act = do
      yield (str "(")
      r <- act
      yield (str ")")
      return r

    -- Read, yield, and pad the file
    yieldFile :: FilePath -> Int64 -> m ()
    yieldFile path fsize = do
      mapM_ yield . BSL.toChunks =<< IO.liftIO (BSL.readFile path)
      yield (BS.replicate (padLen (fromIntegral fsize)) 0)

    strs :: [BS.ByteString] -> BS.ByteString
    strs xs = BS.concat $ str <$> xs

    int :: Integral a => a -> BS.ByteString
    int n = Serial.runPut $ Serial.putInt64le (fromIntegral n)


data IsExecutable = NonExecutable | Executable
    deriving (Eq, Show)

isExecutable :: Functor m => Nar.NarEffects m -> FilePath -> m IsExecutable
isExecutable effs fp =
  bool NonExecutable Executable . Directory.executable <$> Nar.narGetPerms effs fp

-- | Distance to the next multiple of 8
padLen:: Int -> Int
padLen n = (8 - n) `mod` 8
