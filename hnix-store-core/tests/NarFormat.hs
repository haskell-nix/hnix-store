{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NarFormat where

import           Control.Applicative         ((<|>))
import           Control.Concurrent          (threadDelay)
import           Control.Exception           (SomeException, bracket, try)
import           Control.Monad               (replicateM)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Binary                 (put)
import           Data.Binary.Get             (Get (..), runGet)
import           Data.Binary.Put             (Put (..), runPut)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Char8       as BSC
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Lazy.Char8  as BSLC
import           Data.Int
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, isJust)
import qualified Data.Text                   as T
import           GHC.Stats                   (getRTSStats, max_live_bytes)
import           System.Directory            (removeFile)
import           System.Environment          (getEnv)
import qualified System.Process              as P
import           Test.Tasty                  as T
import           Test.Tasty.Hspec
import qualified Test.Tasty.HUnit            as HU
import           Test.Tasty.QuickCheck
import           Text.Read                   (readMaybe)

import           System.Nix.Nar
import           System.Posix.Files         (createSymbolicLink, fileSize, getFileStatus,
                                             isDirectory, readSymbolicLink)
import           System.Directory
import           Data.Bool                  (bool)

-- TODO: Move this to a unix-backed effects library
narEffectsIO :: NarEffects IO
narEffectsIO = NarEffects {
    narReadFile   = BSL.readFile . BSC.unpack
  , narWriteFile  = \f e c -> do
      let f' = BSC.unpack f
      BSL.writeFile f' c
      p <- getPermissions f'
      setPermissions f' (p { executable = e == Executable})
  , narListDir    = (fmap (map (FilePathPart . BSC.pack))) . listDirectory . BSC.unpack
  , narCreateDir  = createDirectory . BSC.unpack
  , narCreateLink = (. BSC.unpack) . createSymbolicLink . BSC.unpack
  , narIsExec     = (fmap (bool NonExecutable Executable . executable)) . getPermissions . BSC.unpack
  , narIsDir      = fmap isDirectory <$> getFileStatus . BSC.unpack
  , narIsSymLink  = pathIsSymbolicLink . BSC.unpack
  , narFileSize   = fmap (fromIntegral . fileSize) <$> getFileStatus . BSC.unpack
  , narReadLink   = (fmap BSC.pack) . readSymbolicLink . BSC.unpack
  }

spec_narEncoding :: Spec
spec_narEncoding = do

  -- For a Haskell embedded Nar, check that (decode . encode === id)
  let roundTrip n = runGet getNar (runPut $ putNar n) `shouldBe` n

  -- For a Haskell embedded Nar, check that encoding it gives
  -- the same bytestring as `nix-store --dump`
  let encEqualsNixStore n b = runPut (putNar n) `shouldBe` b


  describe "parser-roundtrip" $ do
    it "roundtrips regular" $ do
      roundTrip (Nar sampleRegular)

    it "roundtrips regular 2" $ do
      roundTrip (Nar sampleRegular')

    it "roundtrips executable" $ do
      roundTrip (Nar sampleExecutable)

    it "roundtrips symlink" $ do
      roundTrip (Nar sampleSymLink)

    it "roundtrips directory" $ do
      roundTrip (Nar sampleDirectory)


  describe "matches-nix-store fixture" $ do
    it "matches regular" $ do
      encEqualsNixStore (Nar sampleRegular) sampleRegularBaseline

    it "matches regular'" $
      encEqualsNixStore (Nar sampleRegular') sampleRegular'Baseline

    it "matches executable" $
      encEqualsNixStore (Nar sampleExecutable) sampleExecutableBaseline

    it "matches symlink" $
      encEqualsNixStore (Nar sampleSymLink) sampleSymLinkBaseline

    it "matches directory" $ do
      encEqualsNixStore (Nar sampleDirectory) sampleDirectoryBaseline

unit_nixStoreRegular :: HU.Assertion
unit_nixStoreRegular = filesystemNixStore "regular" (Nar sampleRegular)

unit_nixStoreDirectory :: HU.Assertion
unit_nixStoreDirectory = filesystemNixStore "directory" (Nar sampleDirectory)

unit_nixStoreDirectory' :: HU.Assertion
unit_nixStoreDirectory' = filesystemNixStore "directory'" (Nar sampleDirectory')

unit_nixStoreBigFile :: HU.Assertion
unit_nixStoreBigFile = getBigFileSize >>= \sz ->
  filesystemNixStore "bigfile'" (Nar $ sampleLargeFile sz)

unit_nixStoreBigDir :: HU.Assertion
unit_nixStoreBigDir = getBigFileSize >>= \sz ->
  filesystemNixStore "bigfile'" (Nar $ sampleLargeDir sz)

prop_narEncodingArbitrary :: Nar -> Property
prop_narEncodingArbitrary n = runGet getNar (runPut $ putNar n) === n

unit_packSelfSrcDir :: HU.Assertion
unit_packSelfSrcDir = do
  ver <- try (P.readProcess "nix-store" ["--version"] "")
  case ver of
    Left  (e :: SomeException) -> print "No nix-store on system"
    Right _ -> do
      hnixNar <- runPut . put <$> localPackNar narEffectsIO "src"
      nixStoreNar <- getNixStoreDump "src"
      HU.assertEqual
        "src dir serializes the same between hnix-store and nix-store"
        hnixNar
        nixStoreNar

unit_streamLargeFileToNar :: HU.Assertion
unit_streamLargeFileToNar =
  bracket (getBigFileSize >>= makeBigFile) (const rmFiles) $ \_ -> do
    nar <- localPackNar narEffectsIO bigFileName
    BSL.writeFile narFileName . runPut . put $ nar
    assertBoundedMemory
    where
      bigFileName = "bigFile.bin"
      narFileName = "bigFile.nar"
      makeBigFile = \sz -> BSL.writeFile (BSC.unpack bigFileName)
                           (BSL.take sz $ BSL.cycle "Lorem ipsum")
      rmFiles     = removeFile (BSC.unpack bigFileName) >> removeFile narFileName


-- ****************  Utilities  ************************

-- | Generate the ground-truth encoding on the fly with
--   `nix-store --dump`, rather than generating fixtures
--   beforehand
filesystemNixStore :: String -> Nar -> IO ()
filesystemNixStore testErrorName n = do

  ver <- try (P.readProcess "nix-store" ["--version"] "")
  case ver of
    -- Left is not an error - testing machine simply doesn't have
    -- `nix-store` executable, so pass
    Left  (e :: SomeException) -> print "No nix-store on system"
    Right _ ->
      bracket (return ()) (\_ -> P.runCommand "rm -rf testfile nixstorenar.nar hnix.nar") $ \_ -> do

      -- stream nar contents to unpacked file(s)
      localUnpackNar narEffectsIO "testfile" n

      -- nix-store converts those files to nar
      getNixStoreDump "testfile" >>= BSL.writeFile "nixstorenar.nar"

      -- hnix converts those files to nar
      localPackNar narEffectsIO "testfile" >>= BSL.writeFile "hnix.nar" . runPut . putNar

      diffResult <- P.readProcess "diff" ["nixstorenar.nar", "hnix.nar"] ""

      assertBoundedMemory
      HU.assertEqual testErrorName diffResult ""


-- | Assert that GHC uses less than 100M memory at peak
assertBoundedMemory :: IO ()
assertBoundedMemory = do
#ifdef BOUNDED_MEMORY
      bytes <- max_live_bytes <$> getRTSStats
      bytes < 100 * 1000 * 1000 `shouldBe` True
#else
      return ()
#endif


-- | Read the binary output of `nix-store --dump` for a filepath
getNixStoreDump :: String -> IO BSL.ByteString
getNixStoreDump fp = do
  (_,Just h, _, _) <- P.createProcess
                      (P.proc "nix-store" ["--dump", fp])
                      {P.std_out = P.CreatePipe}
  BSL.hGetContents h


-- * Several sample FSOs defined in Haskell, for use in encoding/decoding

-- | Simple regular text file with contents 'hi'
sampleRegular :: FileSystemObject
sampleRegular = Regular NonExecutable 3 "hi\n"

-- | Simple text file with some c code
sampleRegular' :: FileSystemObject
sampleRegular' = Regular NonExecutable (BSL.length str) str
  where str =
          "#include <stdio.h>\n\nint main(int argc, char *argv[]){ exit 0; }\n"

-- | Executable file
sampleExecutable :: FileSystemObject
sampleExecutable = Regular Executable (BSL.length str) str
  where str = "#!/bin/bash\n\ngcc -o hello hello.c\n"

-- | A simple symlink
sampleSymLink :: FileSystemObject
sampleSymLink = SymLink "hello.c"


-- | A directory that includes some of the above sample files
sampleDirectory :: FileSystemObject
sampleDirectory = Directory $ Map.fromList
  [(FilePathPart "hello.c", sampleRegular')
  ,(FilePathPart "build.sh", sampleExecutable)
  ,(FilePathPart "hi.c", sampleSymLink)
  ]

-- | A deeper directory tree with crossing links
sampleDirectory' :: FileSystemObject
sampleDirectory' = Directory $ Map.fromList [

    (FilePathPart "foo", Directory $ Map.fromList [
        (FilePathPart "foo.txt", Regular NonExecutable 8 "foo text")
      , (FilePathPart "tobar"  , SymLink "../bar/bar.txt")
      ])

  , (FilePathPart "bar", Directory $ Map.fromList [
        (FilePathPart "bar.txt", Regular NonExecutable 8 "bar text")
      , (FilePathPart "tofoo"  , SymLink "../foo/foo.txt")
      ])
  ]

sampleLargeFile :: Int64 -> FileSystemObject
sampleLargeFile fSize =
  Regular NonExecutable fSize (BSL.take fSize (BSL.cycle "Lorem ipsum "))


sampleLargeFile' :: Int64 -> FileSystemObject
sampleLargeFile' fSize =
  Regular NonExecutable fSize (BSL.take fSize (BSL.cycle "Lorems ipsums "))

sampleLargeDir :: Int64 -> FileSystemObject
sampleLargeDir fSize = Directory $ Map.fromList $ [
    (FilePathPart "bf1", sampleLargeFile  fSize)
  , (FilePathPart "bf2", sampleLargeFile' fSize)
  ]
  ++ [ (FilePathPart (BSC.pack $ 'f' : show n),
        Regular NonExecutable 10000 (BSL.take 10000 (BSL.cycle "hi ")))
     | n <- [1..100]]
  ++ [
  (FilePathPart "d", Directory $ Map.fromList
      [ (FilePathPart (BSC.pack $ "df" ++ show n)
        , Regular NonExecutable 10000 (BSL.take 10000 (BSL.cycle "subhi ")))
      | n <- [1..100]]
     )
  ]

-- * For each sample above, feed it into `nix-store --dump`,
-- and base64 encode the resulting NAR binary. This lets us
-- check our Haskell NAR generator against `nix-store`

-- "hi" file turned to a NAR with `nix-store --dump`, Base64 encoded
sampleRegularBaseline :: BSL.ByteString
sampleRegularBaseline = B64.decodeLenient $ BSL.concat
  ["DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA"
  ,"AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHJlZ3VsYXIACAAAAA"
  ,"AAAABjb250ZW50cwMAAAAAAAAAaGkKAAAAAAABAAAAAAAAACkAA"
  ,"AAAAAAA"
  ]

sampleRegular'Baseline :: BSL.ByteString
sampleRegular'Baseline = B64.decodeLenient $ BSL.concat
  ["DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA"
  ,"AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHJlZ3VsYXIACAAAAA"
  ,"AAAABjb250ZW50c0AAAAAAAAAAI2luY2x1ZGUgPHN0ZGlvLmg+C"
  ,"gppbnQgbWFpbihpbnQgYXJnYywgY2hhciAqYXJndltdKXsgZXhp"
  ,"dCAwOyB9CgEAAAAAAAAAKQAAAAAAAAA="
  ]

sampleExecutableBaseline :: BSL.ByteString
sampleExecutableBaseline = B64.decodeLenient $ BSL.concat
  ["DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA"
  ,"AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHJlZ3VsYXIACgAAAA"
  ,"AAAABleGVjdXRhYmxlAAAAAAAAAAAAAAAAAAAIAAAAAAAAAGNvb"
  ,"nRlbnRzIgAAAAAAAAAjIS9iaW4vYmFzaAoKZ2NjIC1vIGhlbGxv"
  ,"IGhlbGxvLmMKAAAAAAAAAQAAAAAAAAApAAAAAAAAAA=="
  ]

sampleSymLinkBaseline :: BSL.ByteString
sampleSymLinkBaseline = B64.decodeLenient $ BSL.concat
  ["DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA"
  ,"AAAQAAAAAAAAAdHlwZQAAAAAHAAAAAAAAAHN5bWxpbmsABgAAAA"
  ,"AAAAB0YXJnZXQAAAcAAAAAAAAAaGVsbG8uYwABAAAAAAAAACkAA"
  ,"AAAAAAA"
  ]

sampleDirectoryBaseline :: BSL.ByteString
sampleDirectoryBaseline = B64.decodeLenient $ BSL.concat
  ["DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAA"
  ,"AAAQAAAAAAAAAdHlwZQAAAAAJAAAAAAAAAGRpcmVjdG9yeQAAAA"
  ,"AAAAAFAAAAAAAAAGVudHJ5AAAAAQAAAAAAAAAoAAAAAAAAAAQAA"
  ,"AAAAAAAbmFtZQAAAAAIAAAAAAAAAGJ1aWxkLnNoBAAAAAAAAABu"
  ,"b2RlAAAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAHR5cGUAAAA"
  ,"ABwAAAAAAAAByZWd1bGFyAAoAAAAAAAAAZXhlY3V0YWJsZQAAAA"
  ,"AAAAAAAAAAAAAACAAAAAAAAABjb250ZW50cyIAAAAAAAAAIyEvY"
  ,"mluL2Jhc2gKCmdjYyAtbyBoZWxsbyBoZWxsby5jCgAAAAAAAAEA"
  ,"AAAAAAAAKQAAAAAAAAABAAAAAAAAACkAAAAAAAAABQAAAAAAAAB"
  ,"lbnRyeQAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAG5hbWUAAA"
  ,"AABwAAAAAAAABoZWxsby5jAAQAAAAAAAAAbm9kZQAAAAABAAAAA"
  ,"AAAACgAAAAAAAAABAAAAAAAAAB0eXBlAAAAAAcAAAAAAAAAcmVn"
  ,"dWxhcgAIAAAAAAAAAGNvbnRlbnRzQAAAAAAAAAAjaW5jbHVkZSA"
  ,"8c3RkaW8uaD4KCmludCBtYWluKGludCBhcmdjLCBjaGFyICphcm"
  ,"d2W10peyBleGl0IDA7IH0KAQAAAAAAAAApAAAAAAAAAAEAAAAAA"
  ,"AAAKQAAAAAAAAAFAAAAAAAAAGVudHJ5AAAAAQAAAAAAAAAoAAAA"
  ,"AAAAAAQAAAAAAAAAbmFtZQAAAAAEAAAAAAAAAGhpLmMAAAAABAA"
  ,"AAAAAAABub2RlAAAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAH"
  ,"R5cGUAAAAABwAAAAAAAABzeW1saW5rAAYAAAAAAAAAdGFyZ2V0A"
  ,"AAHAAAAAAAAAGhlbGxvLmMAAQAAAAAAAAApAAAAAAAAAAEAAAAA"
  ,"AAAAKQAAAAAAAAABAAAAAAAAACkAAAAAAAAA"
  ]


-- | Control testcase sizes (bytes) by env variable
getBigFileSize :: IO Int64
getBigFileSize = fromMaybe 1000000 . readMaybe <$> (getEnv "HNIX_BIG_FILE_SIZE" <|> pure "")


-- | Add a link to a FileSystemObject. This is useful
--   when creating Arbitrary FileSystemObjects. It
--   isn't implemented yet
mkLink ::
     FilePath -- ^ Target
  -> FilePath -- ^ Link
  -> FileSystemObject -- ^ FileSystemObject to add link to
  -> FileSystemObject
mkLink = undefined -- TODO


instance Arbitrary Nar where
  arbitrary = Nar <$> resize 10 arbitrary

instance Arbitrary FileSystemObject where
  -- To build an arbitrary Nar,
  arbitrary = do
    n <- getSize
    if n < 2
      then arbFile
      else arbDirectory n

      where

        arbFile :: Gen FileSystemObject
        arbFile = do
          Positive fSize <- arbitrary
          Regular
            <$> elements [NonExecutable, Executable]
            <*> pure (fromIntegral fSize)
            <*> oneof  [
                  fmap (BSL.take fSize . BSL.cycle . BSL.pack . getNonEmpty) arbitrary , -- Binary File
                  fmap (BSL.take fSize . BSL.cycle . BSLC.pack . getNonEmpty) arbitrary   -- ASCII  File
                  ]

        arbName :: Gen FilePathPart
        arbName = fmap (FilePathPart . BS.pack . fmap (fromIntegral . fromEnum)) $ do
          Positive n <- arbitrary
          replicateM n (elements $ ['a'..'z'] ++ ['0'..'9'])

        arbDirectory :: Int -> Gen FileSystemObject
        arbDirectory n = fmap (Directory . Map.fromList) $ replicateM n $ do
          nm <- arbName
          f <- oneof [arbFile, arbDirectory (n `div` 2)]
          return (nm,f)
