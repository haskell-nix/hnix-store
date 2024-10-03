{-# LANGUAGE OverloadedStrings #-}
-- | Stream out a NAR file from a regular file

module System.Nix.Nar.Streamer
  ( NarSource
  , dumpString
  , dumpPath
  , streamNarIO
  , streamNarIOWithOptions
  , Nar.IsExecutable(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Map.Strict                 as Map

import           Control.Monad                    ( forM_
                                                  , when
                                                  )
import qualified Control.Monad.IO.Class          as IO
import qualified Data.ByteString                 as Bytes
import qualified Data.ByteString.Lazy            as Bytes.Lazy
import qualified Data.Foldable
import qualified Data.List
import qualified Data.Serialize                  as Serial
import qualified Data.Text                       as T (pack, unpack)
import qualified Data.Text.Encoding              as TE (encodeUtf8)
import           System.FilePath                 ((</>))

import qualified System.Nix.Nar.Effects as Nar
import qualified System.Nix.Nar.Options as Nar

-- | NarSource
-- The source to provide nar to the handler `(ByteString -> m ())`.
-- It is isomorphic to ByteString by Yoneda lemma
-- if the result is meant to be m ().
-- It is done in CPS style so IO can be chunks.
type NarSource m =  (ByteString -> m ()) -> m ()

-- | dumpString
-- dump a string to nar in CPS style. The function takes in a `ByteString`,
-- and build a `NarSource m`.
dumpString
  :: forall m
   . IO.MonadIO m
  => ByteString -- ^ the string you want to dump
  -> NarSource m -- ^ The nar result in CPS style
dumpString text yield = Data.Foldable.traverse_ (yield . str)
  ["nix-archive-1", "(", "type" , "regular", "contents", text, ")"]

-- | dumpPath
-- shorthand
-- build a Source that turn file path to nar using the default narEffectsIO.
dumpPath
  :: forall m
   . IO.MonadIO m
  => FilePath -- ^ path for the file you want to dump to nar
  -> NarSource m -- ^ the nar result in CPS style
dumpPath = streamNarIO Nar.narEffectsIO

-- | This implementation of Nar encoding takes an arbitrary @yield@
--   function from any streaming library, and repeatedly calls
--   it while traversing the filesystem object to Nar encode
streamNarIO
  :: forall m
   . IO.MonadIO m
  => Nar.NarEffects IO
  -> FilePath
  -> NarSource m
streamNarIO effs basePath yield =
  streamNarIOWithOptions Nar.defaultNarOptions effs basePath yield

streamNarIOWithOptions
  :: forall m
   . IO.MonadIO m
  => Nar.NarOptions
  -> Nar.NarEffects IO
  -> FilePath
  -> NarSource m
streamNarIOWithOptions opts effs basePath yield = do
  yield $ str "nix-archive-1"
  parens $ go basePath
 where
  go :: FilePath -> m ()
  go path = do
    isSymLink <- IO.liftIO $ Nar.narIsSymLink effs path
    if isSymLink then do
      target <- IO.liftIO $ Nar.narReadLink effs path
      yield $
        strs ["type", "symlink", "target", filePathToBS target]
      else do
        isDir <- IO.liftIO $ Nar.narIsDir effs path
        if isDir then do
          fs <- IO.liftIO (Nar.narListDir effs path)
          let entries =
                foldr (\f acc ->
                  let
                    name =
                      if Nar.optUseCaseHack opts
                      then undoCaseHack f
                      else f
                  in Map.insert name f acc
                ) Map.empty fs
          yield $ strs ["type", "directory"]
          forM_ (Map.toAscList entries) $ \(unhacked, original) -> do
            yield $ str "entry"
            parens $ do
              yield $ strs ["name", filePathToBS unhacked, "node"]
              parens $ go (path </> original)
        else do
          isExec <- IO.liftIO $ Nar.narIsExec effs path
          yield $ strs ["type", "regular"]
          when (isExec == Nar.Executable) $ yield $ strs ["executable", ""]
          fSize <- IO.liftIO $ Nar.narFileSize effs path
          yield $ str "contents"
          yield $ int fSize
          yieldFile path fSize

  parens act = do
    yield $ str "("
    r <- act
    yield $ str ")"
    pure r

  -- Read, yield, and pad the file
  yieldFile :: FilePath -> Int64 -> m ()
  yieldFile path fsize = do
    mapM_ yield . Bytes.Lazy.toChunks =<< IO.liftIO (Nar.narReadFile effs path)
    yield $ Bytes.replicate (padLen $ fromIntegral fsize) 0

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

filePathToBS :: FilePath -> ByteString
filePathToBS = TE.encodeUtf8 . T.pack

undoCaseHack :: FilePath -> FilePath
undoCaseHack f =
  case Data.List.findIndex (caseHackSuffix `Data.List.isPrefixOf`) (Data.List.tails f) of
     Just index -> take index f
     Nothing -> f
  where
    caseHackSuffix = T.unpack Nar.caseHackSuffix
