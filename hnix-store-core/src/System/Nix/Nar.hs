{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Description : Allowed effects for interacting with Nar files.
Maintainer  : Shea Levy <shea@shealevy.com>
|-}
module System.Nix.Nar (
    FileSystemObject(..)
  , IsExecutable (..)
  , Nar(..)
  , getNar
  , localPackNar
  , localUnpackNar
  , narEffectsIO
  , putNar
  , FilePathPart(..)
  , filePathPart
  ) where

import           Control.Applicative
import           Control.Monad              (replicateM, replicateM_, (<=<))
import qualified Data.Binary                as B
import qualified Data.Binary.Get            as B
import qualified Data.Binary.Put            as B
import           Data.Bool                  (bool)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as BSL
import           Data.Foldable              (forM_)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           Data.Traversable           (forM)
import           GHC.Int                    (Int64)
import           System.Directory
import           System.FilePath
import           System.Posix.Files         (createSymbolicLink, fileSize, getFileStatus,
                                             isDirectory, readSymbolicLink)


data NarEffects (m :: * -> *) = NarEffects {
    narReadFile   :: FilePath -> m BSL.ByteString
  , narWriteFile  :: FilePath -> BSL.ByteString -> m ()
  , narListDir    :: FilePath -> m [FilePath]
  , narCreateDir  :: FilePath -> m ()
  , narCreateLink :: FilePath -> FilePath -> m ()
  , narGetPerms   :: FilePath -> m Permissions
  , narSetPerms   :: FilePath -> Permissions ->  m ()
  , narIsDir      :: FilePath -> m Bool
  , narIsSymLink  :: FilePath -> m Bool
  , narFileSize   :: FilePath -> m Int64
  , narReadLink   :: FilePath -> m FilePath
}


-- Directly taken from Eelco thesis
-- https://nixos.org/%7Eeelco/pubs/phd-thesis.pdf

data Nar = Nar { narFile :: FileSystemObject }
    deriving (Eq, Show)

-- | A valid filename or directory name
newtype FilePathPart = FilePathPart { unFilePathPart :: BSC.ByteString }
  deriving (Eq, Ord, Show)

-- | Construct FilePathPart from Text by checking that there
--   are no '/' or '\\NUL' characters
filePathPart :: BSC.ByteString -> Maybe FilePathPart
filePathPart p = case BSC.any (`elem` ['/', '\NUL']) p of
  False -> Just $ FilePathPart p
  True  -> Nothing

-- | A FileSystemObject (FSO) is an anonymous entity that can be NAR archived
data FileSystemObject =
    Regular IsExecutable Int64 BSL.ByteString
    -- ^ Reguar file, with its executable state, size (bytes) and contents
  | Directory (Map.Map FilePathPart FileSystemObject)
    -- ^ Directory with mapping of filenames to sub-FSOs
  | SymLink T.Text
    -- ^ Symbolic link target
  deriving (Eq, Show)


data IsExecutable = NonExecutable | Executable
    deriving (Eq, Show)


instance B.Binary Nar where
  get = getNar
  put = putNar

------------------------------------------------------------------------------
-- | Serialize Nar to lazy ByteString
putNar :: Nar -> B.Put
putNar (Nar file) = header <> parens (putFile file)
    where

        header   = str "nix-archive-1"

        putFile (Regular isExec fSize contents) =
               strs ["type", "regular"]
            >> (if isExec == Executable
               then strs ["executable", ""]
               else return ())
            >> putContents fSize contents

        putFile (SymLink target) =
               strs ["type", "symlink", "target", BSL.fromStrict $ E.encodeUtf8 target]

        -- toList sorts the entries by FilePathPart before serializing
        putFile (Directory entries) =
               strs ["type", "directory"]
            <> mapM_ putEntry (Map.toList entries)

        putEntry (FilePathPart name, fso) = do
            str "entry"
            parens $ do
              str "name"
              str (BSL.fromStrict name)
              str "node"
              parens (putFile fso)

        parens m = str "(" >> m >> str ")"

        -- Do not use this for file contents
        str :: BSL.ByteString -> B.Put
        str t = let len = BSL.length t
            in int len <> pad len t

        putContents :: Int64 -> BSL.ByteString -> B.Put
        putContents fSize bs = str "contents" <> int fSize <> (pad fSize bs)
        -- putContents fSize bs = str "contents" <> int (BSL.length bs) <> (pad fSize bs)

        int :: Integral a => a -> B.Put
        int n = B.putInt64le $ fromIntegral n

        pad :: Int64 -> BSL.ByteString -> B.Put
        pad strSize bs = do
          B.putLazyByteString bs
          B.putLazyByteString (BSL.replicate (padLen strSize) 0)

        strs :: [BSL.ByteString] -> B.Put
        strs = mapM_ str


------------------------------------------------------------------------------
-- | Deserialize a Nar from lazy ByteString
getNar :: B.Get Nar
getNar = fmap Nar $ header >> parens getFile
    where

      header   = assertStr "nix-archive-1"


      -- Fetch a FileSystemObject
      getFile = getRegularFile <|> getDirectory <|> getSymLink

      getRegularFile = do
          assertStr "type"
          assertStr "regular"
          mExecutable <- optional $ Executable <$ (assertStr "executable"
                                                   >> assertStr "")
          assertStr "contents"
          (fSize, contents) <- sizedStr
          return $ Regular (fromMaybe NonExecutable mExecutable) fSize contents

      getDirectory = do
          assertStr "type"
          assertStr "directory"
          fs <- many getEntry
          return $ Directory (Map.fromList fs)

      getSymLink = do
          assertStr "type"
          assertStr "symlink"
          assertStr "target"
          fmap (SymLink . E.decodeUtf8 . BSL.toStrict) str

      getEntry = do
          assertStr "entry"
          parens $ do
              assertStr "name"
              name <- E.decodeUtf8 . BSL.toStrict <$> str
              assertStr "node"
              file <- parens getFile
              maybe (fail $ "Bad FilePathPart: " ++ show name)
                    (return . (,file))
                    (filePathPart $ E.encodeUtf8 name)

      -- Fetch a length-prefixed, null-padded string
      str = fmap snd sizedStr

      sizedStr = do
          n <- B.getInt64le
          s <- B.getLazyByteString n
          p <- B.getByteString . fromIntegral $ padLen n
          return (n,s)

      parens m = assertStr "(" *> m <* assertStr ")"

      assertStr s = do
          s' <- str
          if s == s'
              then return s
              else fail "No"


-- | Distance to the next multiple of 8
padLen :: Int64 -> Int64
padLen n = (8 - n) `mod` 8


-- | Unpack a NAR into a non-nix-store directory (e.g. for testing)
localUnpackNar :: Monad m => NarEffects m -> FilePath -> Nar -> m ()
localUnpackNar effs basePath (Nar fso) = localUnpackFSO basePath fso

  where

    localUnpackFSO basePath fso = case fso of

       Regular isExec _ bs -> do
         (narWriteFile effs) basePath bs
         p <- narGetPerms effs basePath
         (narSetPerms effs) basePath (p {executable = isExec == Executable})

       SymLink targ -> narCreateLink effs (T.unpack targ) basePath

       Directory contents -> do
         narCreateDir effs basePath
         forM_ (Map.toList contents) $ \(FilePathPart path', fso) ->
           localUnpackFSO (basePath </> BSC.unpack path') fso


-- | Pack a NAR from a filepath
localPackNar :: Monad m => NarEffects m -> FilePath -> m Nar
localPackNar effs basePath = Nar <$> localPackFSO basePath

  where

    localPackFSO path' = do
      fType <- (,) <$> narIsDir effs path' <*> narIsSymLink effs path'
      case fType of
        (_,  True) -> SymLink . T.pack <$> narReadLink effs path'
        (False, _) -> Regular <$> isExecutable effs path'
                              <*> narFileSize effs path'
                              <*> narReadFile effs path'
        (True , _) -> fmap (Directory . Map.fromList) $ do
          fs <- narListDir effs path'
          forM fs $ \fp ->
            (FilePathPart (BSC.pack $  fp),) <$> localPackFSO (path' </> fp)



narEffectsIO :: NarEffects IO
narEffectsIO = NarEffects {
    narReadFile   = BSL.readFile
  , narWriteFile  = BSL.writeFile
  , narListDir    = listDirectory
  , narCreateDir  = createDirectory
  , narCreateLink = createSymbolicLink
  , narGetPerms   = getPermissions
  , narSetPerms   = setPermissions
  , narIsDir      = fmap isDirectory <$> getFileStatus
  , narIsSymLink  = pathIsSymbolicLink
  , narFileSize   = fmap (fromIntegral . fileSize) <$> getFileStatus
  , narReadLink   = readSymbolicLink
  }


isExecutable :: Functor m => NarEffects m -> FilePath -> m IsExecutable
isExecutable effs fp =
  bool NonExecutable Executable . executable <$> narGetPerms effs fp
