{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description : Allowed effects for interacting with Nar files.
Maintainer  : Shea Levy <shea@shealevy.com>
|-}
module System.Nix.Nar where

import           Control.Monad (replicateM, replicateM_)
import           Data.Monoid ((<>))
import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Set as Set
import qualified Data.Binary as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Binary.Put as B
import qualified Data.Binary.Get as B
import           Debug.Trace

import System.Nix.Path


data NarEffects (m :: * -> *) = NarEffets {
    readFile         :: FilePath -> m BSL.ByteString
  , listDir          :: FilePath -> m [FileSystemObject]
  , narFromFileBytes :: BSL.ByteString -> m Nar
  , narFromDirectory :: FilePath -> m Nar
}


-- Directly taken from Eelco thesis
-- https://nixos.org/%7Eeelco/pubs/phd-thesis.pdf

-- TODO: Should we use rootedPath, validPath rather than FilePath?
data Nar = Nar { narFile :: FileSystemObject }
    deriving (Eq, Ord, Show)

data FileSystemObject =
    Regular IsExecutable BSL.ByteString
  | Directory (Set.Set (PathName, FileSystemObject))
  | SymLink BSL.ByteString
  deriving (Eq, Show)

-- TODO - is this right? How does thesis define ordering of FSOs?
instance Ord FileSystemObject where
    compare (Regular _ c1) (Regular _ c2) = compare c1 c2
    compare (Regular _ _)  _              = GT
    compare (Directory s1) (Directory s2) = compare s1 s2
    compare (Directory _)  _              = GT
    compare (SymLink l1) (SymLink l2)     = compare l1 l2

data IsExecutable = NonExecutable | Executable
    deriving (Eq, Show)

-- data NarFile = NarFile
--     { narFileIsExecutable :: IsExecutable
--     , narFilePath         :: FilePath -- TODO: Correct type?
--     } deriving (Show)

data DebugPut = PutAscii | PutBinary

putNar :: Nar -> B.Put
putNar = putNar' PutBinary

putNar' :: DebugPut -> Nar -> B.Put
putNar' dbg (Nar file) = header <>
                         parens (putFile file)
    where

        str' = case dbg of
            PutAscii -> strDebug
            PutBinary -> str

        header   = str' "nix-archive-1"
        parens m = str' "(" <> m <> str ")"

        putFile (Regular isExec contents) =
               str' "type" <> str' "regular"
            <> if isExec == Executable
               then str' "executable" <> str' ""
               else str' ""
            <> str' "contents" <> str' contents

        putFile (SymLink target) =
               str' "type" <> str' "symlink" <> str' "target" <> str' target

        putFile (Directory entries) =
               str' "type" <> str' "directory"
            <> foldMap putEntry entries

        putEntry (PathName name, fso) =
            str' "entry" <>
            parens (str' "name" <>
                    str' (BSL.fromStrict $ E.encodeUtf8 name) <>
                    str' "node" <>
                    putFile fso)

getNar :: B.Get Nar
getNar = fmap Nar $ header >> parens getFile
    where header   = trace "header " $ assertStr "nix-archive-1"

          padLen n = let r = n `mod` 8
                         p = (8 - n) `mod` 8
                     in trace ("padLen: " ++ show p) p

          str = do
              n <- fmap fromIntegral B.getInt64le
              s <- B.getLazyByteString n
              p <- B.getByteString (padLen $ fromIntegral n)
              traceShow (n,s) $ return s

          assertStr s = trace ("Assert " ++ show s) $ do
              s' <- str
              if s == s'
                  then trace ("Assert " ++ show s ++ " passed") (return s)
                  else trace ("Assert " ++ show s ++ " failed") (fail "No")

          parens m = assertStr "(" *> m <* assertStr ")"

          getFile :: B.Get FileSystemObject
          getFile = trace "getFile" (getRegularFile)
                <|> trace "getDir" (getDirectory)
                <|> trace "getLink" (getSymLink)

          getRegularFile = trace "regular" $ do
              trace "TESTING" (assertStr "type")
              trace "HI" $ assertStr "regular"
              trace "HI AGOIN" $ assertStr "contents"
              contents <- str
              return $ Regular (maybe NonExecutable
                                   (const Executable) Nothing) contents

          getDirectory = do
              assertStr "type"
              assertStr "directory"
              fs <- many getEntry
              return $ Directory (Set.fromList fs)

          getSymLink = do
              assertStr "type"
              assertStr "symlink"
              assertStr "target"
              fmap SymLink str

          getEntry = do
              assertStr "entry"
              parens $ do
                  assertStr "name"
                  mname <- pathName . E.decodeUtf8 . BSL.toStrict <$> str
                  assertStr "node"
                  file <- parens getFile
                  maybe (fail "Bad PathName") (return . (,file)) mname

str :: BSL.ByteString -> B.Put
str t = let len = BSL.length t
    in int len <> pad t

int :: Integral a => a -> B.Put
int n = B.putInt64le $ fromIntegral n

pad :: BSL.ByteString -> B.Put
pad bs =
    let padLen = BSL.length bs `div` 8
    in  B.put bs >> B.put (BSL.replicate padLen '\NUL')

strDebug :: BSL.ByteString -> B.Put
strDebug t = let len = BSL.length t
    in intDebug len <> padDebug t

intDebug :: Integral a => a -> B.Put
intDebug a = B.put (show @Int (fromIntegral a))

padDebug :: BSL.ByteString -> B.Put
padDebug bs =
    let padLen = BSL.length bs `div` 8
    in  B.put bs >> B.put (BSL.replicate padLen '_')
