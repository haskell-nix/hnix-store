-- | Stream out a NAR file from a regular file

{-# language ScopedTypeVariables #-}

module System.Nix.Internal.Nar.Streamer
  ( NarSource 
  , dumpString
  , dumpPath
  , streamNarIO
  , IsExecutable(..)
  )
where

import qualified Control.Monad.IO.Class          as IO
import qualified Data.ByteString                 as Bytes
import qualified Data.ByteString.Char8           as Bytes.Char8
import qualified Data.ByteString.Lazy            as Bytes.Lazy
import qualified Data.Serialize                  as Serial
import qualified System.Directory                as Directory
import           System.FilePath                  ( (</>) )

import qualified System.Nix.Internal.Nar.Effects as Nar


-- | NarSource 
-- the source to provide nar to the handler
type NarSource m =  (ByteString -> m ()) -> m ()


-- | dumpString
-- dump a string to nar
dumpString :: forall m . IO.MonadIO m => ByteString -> NarSource m
dumpString text yield = do
  yield $ str "nix-archive-1"
  yield $ str "("
  yield $ str "type" 
  yield $ str "regular"
  yield $ str "contents"
  yield $ str text
  yield $ str ")"


-- | dumpPath
-- shorthand
-- build a Source that turn file path to nar using the default narEffectsIO.
dumpPath :: forall m . IO.MonadIO m => FilePath -> NarSource m
dumpPath = streamNarIO Nar.narEffectsIO


-- | This implementation of Nar encoding takes an arbitrary @yield@
--   function from any streaming library, and repeatedly calls
--   it while traversing the filesystem object to Nar encode
streamNarIO :: forall m . IO.MonadIO m => Nar.NarEffects IO -> FilePath -> NarSource m
streamNarIO effs basePath yield = do
  yield $ str "nix-archive-1"
  yield $ str "("
  go basePath
  yield $ str ")"
 where
  go :: FilePath -> m ()
  go path = do
    isDir     <- IO.liftIO $ Nar.narIsDir effs path
    isSymLink <- IO.liftIO $ Nar.narIsSymLink effs path
    let isRegular = not $ isDir || isSymLink

    when isSymLink $ do
      target <- IO.liftIO $ Nar.narReadLink effs path
      yield $
        strs ["type", "symlink", "target", Bytes.Char8.pack target]

    when isRegular $ do
      isExec <- IO.liftIO $ isExecutable effs path
      yield $ strs ["type", "regular"]
      when (isExec == Executable) $ yield $ strs ["executable", ""]
      fSize <- IO.liftIO $ Nar.narFileSize effs path
      yield $ str "contents"
      yield $ int fSize
      yieldFile path fSize

    when isDir $ do
      fs <- IO.liftIO (Nar.narListDir effs path)
      yield $ strs ["type", "directory"]
      forM_ (sort fs) $ \f -> do
        yield $ str "entry"
        parens $ do
          let fullName = path </> f
          yield $ strs ["name", Bytes.Char8.pack f, "node"]
          parens $ go fullName

  parens act = do
    yield $ str "("
    r <- act
    yield $ str ")"
    pure r

  -- Read, yield, and pad the file
  yieldFile :: FilePath -> Int64 -> m ()
  yieldFile path fsize = do
    mapM_ yield . Bytes.Lazy.toChunks =<< IO.liftIO (Bytes.Lazy.readFile path)
    yield $ Bytes.replicate (padLen $ fromIntegral fsize) 0
          

data IsExecutable = NonExecutable | Executable
  deriving (Eq, Show)

isExecutable :: Functor m => Nar.NarEffects m -> FilePath -> m IsExecutable
isExecutable effs fp =
  bool
    NonExecutable
    Executable
    . Directory.executable <$> Nar.narGetPerms effs fp

-- | Distance to the next multiple of 8
padLen :: Int -> Int
padLen n = (8 - n) `mod` 8

int :: Integral a => a -> ByteString
int n = Serial.runPut $ Serial.putInt64le $ fromIntegral n

str :: ByteString -> ByteString
str t =
  let
    len = Bytes.length t
  in
    int len <> padBS len t

padBS :: Int -> ByteString -> ByteString
padBS strSize bs = bs <> Bytes.replicate (padLen strSize) 0

strs :: [ByteString] -> ByteString
strs xs = Bytes.concat $ str <$> xs