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
import Data.Map.Strict qualified                 as Map

import           Control.Monad                    ( forM
                                                  , forM_
                                                  , when
                                                  )
import Control.Monad.IO.Class qualified          as IO
import Data.ByteString qualified                 as Bytes
import Data.ByteString.Lazy qualified            as Bytes.Lazy
import Data.Foldable qualified
import Data.List qualified
import Data.Serialize qualified                  as Serial
import Data.Text qualified                       as T (unpack)
import GHC.Foreign qualified                     as Foreign
import GHC.IO.Encoding qualified                 as Encoding
import           System.FilePath                 ((</>))
import           System.IO                       (TextEncoding)

import System.Nix.Nar.Effects qualified as Nar
import System.Nix.Nar.Options qualified as Nar

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
  fileSystemEncoding <- IO.liftIO Encoding.getFileSystemEncoding
  yield $ str "nix-archive-1"
  parens $ go fileSystemEncoding basePath
 where
  go :: TextEncoding -> FilePath -> m ()
  go fileSystemEncoding path = do
    isSymLink <- IO.liftIO $ Nar.narIsSymLink effs path
    if isSymLink then do
      target <- IO.liftIO $ Nar.narReadLink effs path
      targetBytes <- IO.liftIO $ filePathToBS fileSystemEncoding target
      yield $
        strs ["type", "symlink", "target", targetBytes]
      else do
        isDir <- IO.liftIO $ Nar.narIsDir effs path
        if isDir then do
          fs <- IO.liftIO (Nar.narListDir effs path)
          names <- IO.liftIO $ forM fs $ \f -> do
            let name =
                  if Nar.optUseCaseHack opts
                  then undoCaseHack f
                  else f
            nameBytes <- filePathToBS fileSystemEncoding name
            pure (nameBytes, name, f)
          let entries =
                foldr (\(nameBytes, name, original) acc ->
                  case Map.insertLookupWithKey (\_ n _ -> n) nameBytes (name, original) acc of
                    (Nothing, newMap) -> newMap
                    (Just (conflict, _), _) -> error $ "File name collision between " ++ (path </> name) ++ " and " ++ (path </> conflict)
                ) Map.empty names
          yield $ strs ["type", "directory"]
          forM_ (Map.toAscList entries) $ \(nameBytes, (_, original)) -> do
            yield $ str "entry"
            parens $ do
              yield $ strs ["name", nameBytes, "node"]
              parens $ go fileSystemEncoding (path </> original)
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

filePathToBS :: TextEncoding -> FilePath -> IO ByteString
filePathToBS encoding filePath =
  Foreign.withCStringLen encoding filePath Bytes.packCStringLen

undoCaseHack :: FilePath -> FilePath
undoCaseHack f =
  case Data.List.findIndex (caseHackSuffix `Data.List.isPrefixOf`) (Data.List.tails f) of
     Just index -> take index f
     Nothing -> f
  where
    caseHackSuffix = T.unpack Nar.caseHackSuffix
