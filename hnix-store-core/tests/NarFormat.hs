{-# language CPP                 #-}
{-# language ScopedTypeVariables #-}

module NarFormat where

import qualified Control.Concurrent               as Concurrent
import           Control.Exception                (try)
import           Data.Binary                      (Binary(..), decodeFile)
import           Data.Binary.Get                  (Get, getByteString,
                                                   getInt64le,
                                                   getLazyByteString, runGet)
import           Data.Binary.Put                  (Put, putInt64le,
                                                   putLazyByteString, runPut)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base64.Lazy      as B64
import qualified Data.ByteString.Char8            as BSC
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Lazy.Char8       as BSLC
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           System.Directory                 ( doesDirectoryExist
                                                  , doesPathExist
                                                  , removeDirectoryRecursive
                                                  , removeFile
                                                  )
import qualified System.Directory                 as Directory
import           System.Environment               (getEnv)
import           System.FilePath                  ((<.>), (</>))
import qualified System.IO                        as IO
import qualified System.IO.Temp                   as Temp
import qualified System.Posix.Process             as Unix
import qualified System.Process                   as P
import           Test.Tasty                       as T
import           Test.Hspec
import qualified Test.Tasty.HUnit                 as HU
import           Test.Tasty.QuickCheck
import qualified Text.Printf                      as Printf

import qualified System.Nix.Internal.Nar.Streamer as Nar
import           System.Nix.Nar

-- Without the import, `max_live_bytes` and `getRTSStats` are undefined on some setups.
#ifdef BOUNDED_MEMORY
import GHC.Stats
#endif


withBytesAsHandle :: BSLC.ByteString -> (IO.Handle -> IO a) -> IO a
withBytesAsHandle bytes act = do
  Temp.withSystemTempFile "nar-test-file-XXXXX" $ \tmpFile h -> do
    IO.hClose h
    BSL.writeFile tmpFile bytes
    withFile tmpFile ReadMode act

spec_narEncoding :: Spec
spec_narEncoding = do

  -- For a Haskell embedded Nar, check that (decode . encode === id)
  let
    withTempDir act = Temp.withSystemTempDirectory "nar-test" act


    roundTrip :: String -> Nar -> IO ()
    roundTrip narFileName n = withTempDir $ \tmpDir -> do
        let packageFilePath = tmpDir </> narFileName

        e <- doesPathExist packageFilePath
        e `shouldBe` False

        res <- withBytesAsHandle (runPut (putNar n)) $ \h -> do
          unpackNarIO narEffectsIO h packageFilePath
        res `shouldBe` pass

        e' <- doesPathExist packageFilePath
        e' `shouldBe` True

        res' <- Temp.withSystemTempFile "nar-test-file-hnix" $ \tmpFile h -> do
          buildNarIO narEffectsIO packageFilePath h
          IO.hClose h
          BSL.readFile tmpFile

        res' `shouldBe` runPut (putNar n)

  -- For a Haskell embedded Nar, check that encoding it gives
  -- the same bytestring as `nix-store --dump`
  let
    encEqualsNixStore :: Nar -> BSL.ByteString -> IO ()
    encEqualsNixStore n b = runPut (putNar n) `shouldBe` b


  describe "parser-roundtrip" $ do
    it "roundtrips regular" $ do
      roundTrip "sampleRegular" (Nar sampleRegular)

    it "roundtrips regular 2" $ do
      roundTrip "sampleRegular'" (Nar sampleRegular')

    it "roundtrips executable" $ do
      roundTrip "sampleExecutable" (Nar sampleExecutable)

    it "roundtrips directory" $ do
      roundTrip "sampleDirectory" (Nar sampleDirectory)

    it "roundtrips case conflicts" $ do
      nar <- decodeFile "tests/fixtures/case-conflict.nar"
      roundTrip "caseConflict" nar

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

    it "matches symlink to directory" $ do
      encEqualsNixStore (Nar sampleLinkToDirectory) sampleLinkToDirectoryBaseline


unit_nixStoreRegular :: HU.Assertion
unit_nixStoreRegular = filesystemNixStore "regular" (Nar sampleRegular)

unit_nixStoreDirectory :: HU.Assertion
unit_nixStoreDirectory = filesystemNixStore "directory" (Nar sampleDirectory)

unit_nixStoreDirectory' :: HU.Assertion
unit_nixStoreDirectory' = filesystemNixStore "directory'" (Nar sampleDirectory')

test_nixStoreBigFile :: TestTree
test_nixStoreBigFile = packThenExtract "bigfile" $ \baseDir -> do
  mkBigFile (baseDir </> "bigfile")


test_nixStoreBigDir :: TestTree
test_nixStoreBigDir = packThenExtract "bigdir" $ \baseDir -> do
  let testDir = baseDir </> "bigdir"
  Directory.createDirectory testDir
  mkBigFile (testDir </> "bf1")
  mkBigFile (testDir </> "bf2")
  -- flip mapM_ [1..100] $ \i ->
  --   mkBigFile (testDir </> ('f': show i))
  -- -- Directory.createDirectory (testDir </> "")


prop_narEncodingArbitrary :: Nar -> Property
prop_narEncodingArbitrary n = runGet getNar (runPut $ putNar n) === n

unit_packSelfSrcDir :: HU.Assertion
unit_packSelfSrcDir = Temp.withSystemTempDirectory "nar-test" $ \tmpDir -> do
  ver <- try (P.readProcess "nix-store" ["--version"] "")
  let narFilePath = tmpDir </> "src.nar"
  case ver of
    Left  (_ :: SomeException) -> print ("No nix-store on system" :: String)
    Right _ -> do
      let go dir = do
            srcHere <- doesDirectoryExist dir
            bool
              pass
              (do
                withFile narFilePath WriteMode $ \h ->
                  buildNarIO narEffectsIO "src" h
                hnixNar <- BSL.readFile narFilePath
                nixStoreNar <- getNixStoreDump "src"
                HU.assertEqual
                  "src dir serializes the same between hnix-store and nix-store"
                  hnixNar
                  nixStoreNar
              )
              srcHere
      go "src"
      go "hnix-store-core/src"
-- ||||||| merged common ancestors
--       hnixNar <- runPut . put <$> localPackNar narEffectsIO "src"
--       nixStoreNar <- getNixStoreDump "src"
--       HU.assertEqual
--         "src dir serializes the same between hnix-store and nix-store"
--         hnixNar
--         nixStoreNar
-- =======
--       let narFile = tmpDir </> "src.nar"
--       withFile narFile WriteMode $ \h ->
--         buildNarIO narEffectsIO "src" h
--       hnixNar <- BSL.readFile narFile
--       nixStoreNar <- getNixStoreDump "src"
--       HU.assertEqual
--         "src dir serializes the same between hnix-store and nix-store"
--         hnixNar
--         nixStoreNar
-- >>>>>>> Use streaming to consume and produce NARs

-- passes
test_streamLargeFileToNar :: TestTree
test_streamLargeFileToNar = HU.testCaseSteps "streamLargeFileToNar" $ \step -> do

  step "create test file"
  mkBigFile bigFileName
  -- BSL.writeFile narFileName =<< buildNarIO narEffectsIO bigFileName
  --
  step "create nar file"
  withFile narFileName WriteMode $ \h ->
    buildNarIO narEffectsIO bigFileName h

  step "assert bounded memory"
  assertBoundedMemory
  rmFiles
    where
      bigFileName = "bigFile.bin"
      narFileName = "bigFile.nar"
      rmFiles     = removeFile bigFileName >> removeFile narFileName


--------------------------------------------------------------------------------
test_streamManyFilesToNar :: TestTree
test_streamManyFilesToNar = HU.testCaseSteps "streamManyFilesToNar" $ \step ->
  Temp.withSystemTempDirectory "hnix-store" $ \baseDir -> do
    let

      packagePath = baseDir </> "package_with_many_files"
      packagePath' = baseDir </> "package_with_many_files2"
      narFilePath = packagePath <.> "nar"

      -- unused, see `step "check file exists"` bellow
      _rmFiles = try @SomeException @() $ do
        e <- doesPathExist narFilePath
        when e $ removeDirectoryRecursive narFilePath

      _run =  do
        filesPrecount <- countProcessFiles
        withFile "hnar" WriteMode $ \h ->
          buildNarIO narEffectsIO narFilePath h
        filesPostcount <- countProcessFiles
        pure $ (-) <$> filesPostcount <*> filesPrecount

    step "create test files"
    Directory.createDirectory packagePath
    forM_ [0..1000] $ \i -> do
      BSL.writeFile (Printf.printf (packagePath </> "%08d") (i :: Int)) "hi\n"
      Concurrent.threadDelay 50

    filesPrecount <- countProcessFiles

    step "pack nar"
    withFile narFilePath WriteMode $ \h ->
      buildNarIO narEffectsIO packagePath h

    step "unpack nar"
    r <- withFile narFilePath ReadMode $ \h ->
      unpackNarIO narEffectsIO h packagePath'
    r `shouldBe` pass

    step "check constant file usage"
    filesPostcount <- countProcessFiles
    case (-) <$> filesPostcount <*> filesPrecount of
      Nothing -> pass
      Just c -> c `shouldSatisfy` (< 50)

    -- step "check file exists"
    -- e <- doesPathExist packagePath'
    -- e `shouldBe` True

    -- step "read the NAR back in"
    -- filesCreated <- run `finally` rmFiles
    -- filesCreated `shouldSatisfy` (< 50)



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
    Left  (_ :: SomeException) -> print ("No nix-store on system" :: String)
    Right _ -> Temp.withSystemTempDirectory "hnix-store" $ \baseDir -> do

      let
        testFile    = baseDir </> "testfile"
        nixNarFile  = baseDir </> "nixstorenar.nar"
        hnixNarFile = baseDir </> "hnix.nar"

        assertExists f = do
          e <- doesPathExist f
          e `shouldBe` True

      -- stream nar contents to unpacked file(s)
      void $ withBytesAsHandle (runPut $ putNar n) $ \h ->
        unpackNarIO narEffectsIO h testFile

      assertExists testFile

      -- nix-store converts those files to nar
      getNixStoreDump testFile >>= BSL.writeFile nixNarFile
      assertExists nixNarFile

      -- hnix converts those files to nar
      withFile hnixNarFile WriteMode $ \h ->
        buildNarIO narEffectsIO testFile h
      assertExists hnixNarFile

      diffResult <- P.readProcess "diff" [nixNarFile, hnixNarFile] ""

      assertBoundedMemory
      HU.assertEqual testErrorName diffResult ""


-- | Assert that GHC uses less than 100M memory at peak
assertBoundedMemory :: IO ()
assertBoundedMemory = do
#ifdef BOUNDED_MEMORY
      bytes <- max_live_bytes <$> getRTSStats
      bytes < 100 * 1000 * 1000 `shouldBe` True
#else
      pass
#endif


packThenExtract
  :: String
     -- ^ Test name (will also be used for file name)
  -> (String -> IO ())
     -- ^ Action to create some files that we will
     --   pack into a NAR
  -> TestTree
packThenExtract testName setup =
  HU.testCaseSteps testName $ \step ->
  Temp.withSystemTempDirectory "hnix-store" $ \baseDir -> do
    setup baseDir

    let narFilePath = baseDir </> testName

    ver <- try (P.readProcess "nix-store" ["--version"] "")
    case ver of
      Left (_ :: SomeException) -> print ("No nix-store on system" :: String)
      Right _ -> do
        let
          nixNarFile  = narFilePath <> ".nix"
          hnixNarFile = narFilePath <> ".hnix"
          outputFile  = narFilePath <> ".out"

        step $ "Produce nix-store nar to " <> nixNarFile
        (_,_,_,handle) <- P.createProcess (P.shell $ "nix-store --dump " <> narFilePath <> " > " <> nixNarFile)
        void $ P.waitForProcess handle

        step $ "Build NAR from " <> narFilePath <> " to " <> hnixNarFile
        -- narBS <- buildNarIO narEffectsIO narFile
        withFile hnixNarFile WriteMode $ \h ->
          buildNarIO narEffectsIO narFilePath h

        -- BSL.writeFile hnixNarFile narBS

        step $ "Unpack NAR to " <> outputFile
        _narHandle <- withFile nixNarFile ReadMode $ \h ->
          unpackNarIO narEffectsIO h outputFile

        pass

-- | Count file descriptors owned by the current process
countProcessFiles :: IO (Maybe Int)
countProcessFiles = do
  pid <- Unix.getProcessID
  hasProc <- doesDirectoryExist "/proc"
  if not hasProc
    then pure Nothing
    else do
      let fdDir = "/proc/" <> show pid <> "/fd"
      fds  <- toText <$> P.readProcess "ls" [fdDir] ""
      pure $ pure $ length $ words fds


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
sampleRegular = Regular Nar.NonExecutable 3 "hi\n"

-- | Simple text file with some c code
sampleRegular' :: FileSystemObject
sampleRegular' = Regular Nar.NonExecutable (BSL.length str) str
  where str =
          "#include <stdio.h>\n\nint main(int argc, char *argv[]){ exit 0; }\n"

-- | Executable file
sampleExecutable :: FileSystemObject
sampleExecutable = Regular Nar.Executable (BSL.length str) str
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
        (FilePathPart "foo.txt", Regular Nar.NonExecutable 8 "foo text")
      , (FilePathPart "tobar"  , SymLink "../bar/bar.txt")
      ])

  , (FilePathPart "bar", Directory $ Map.fromList [
        (FilePathPart "bar.txt", Regular Nar.NonExecutable 8 "bar text")
      , (FilePathPart "tofoo"  , SymLink "../foo/foo.txt")
      ])
  ]

sampleLargeFile :: Int64 -> FileSystemObject
sampleLargeFile fSize =
  Regular Nar.NonExecutable fSize (BSL.take fSize (BSL.cycle "Lorem ipsum "))


sampleLargeFile' :: Int64 -> FileSystemObject
sampleLargeFile' fSize =
  Regular Nar.NonExecutable fSize (BSL.take fSize (BSL.cycle "Lorems ipsums "))

sampleLargeDir :: Int64 -> FileSystemObject
sampleLargeDir fSize = Directory $ Map.fromList $ [
    (FilePathPart "bf1", sampleLargeFile  fSize)
  , (FilePathPart "bf2", sampleLargeFile' fSize)
  ]
  <> [ (FilePathPart (BSC.pack $ 'f' : show n),
        Regular Nar.NonExecutable 10000 (BSL.take 10000 (BSL.cycle "hi ")))
     | n <- [1..100 :: Int]]
  <> [
  (FilePathPart "d", Directory $ Map.fromList
      [ (FilePathPart (BSC.pack $ "df" <> show n)
        , Regular Nar.NonExecutable 10000 (BSL.take 10000 (BSL.cycle "subhi ")))
      | n <- [1..100 :: Int]]
     )
  ]

sampleLinkToDirectory :: FileSystemObject
sampleLinkToDirectory = Directory $ Map.fromList [
  (FilePathPart "foo", Directory $ Map.fromList [
        (FilePathPart "file", Regular Nar.NonExecutable 8 "foo text")
      ])
  , (FilePathPart "linkfoo"  , SymLink "foo")
  ]

--------------------------------------------------------------------------------
sampleDirWithManyFiles :: Int -> FileSystemObject
sampleDirWithManyFiles nFiles =
  Directory $ Map.fromList $ mkFile <$> take nFiles [0..]
  where
    mkFile :: Int -> (FilePathPart, FileSystemObject)
    mkFile i = (FilePathPart (BSC.pack (Printf.printf "%08d" i)),
                sampleRegular)

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

sampleLinkToDirectoryBaseline :: BSL.ByteString
sampleLinkToDirectoryBaseline = B64.decodeLenient $ BSL.concat
  ["DQAAAAAAAABuaXgtYXJjaGl2ZS0xAAAAAQAAAAAAAAAoAAAAAAAAAAQAAAAAAAAAdHlwZQAAAAAJ"
  ,"AAAAAAAAAGRpcmVjdG9yeQAAAAAAAAAFAAAAAAAAAGVudHJ5AAAAAQAAAAAAAAAoAAAAAAAAAAQA"
  ,"AAAAAAAAbmFtZQAAAAADAAAAAAAAAGZvbwAAAAAABAAAAAAAAABub2RlAAAAAAEAAAAAAAAAKAAA"
  ,"AAAAAAAEAAAAAAAAAHR5cGUAAAAACQAAAAAAAABkaXJlY3RvcnkAAAAAAAAABQAAAAAAAABlbnRy"
  ,"eQAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAG5hbWUAAAAABAAAAAAAAABmaWxlAAAAAAQAAAAA"
  ,"AAAAbm9kZQAAAAABAAAAAAAAACgAAAAAAAAABAAAAAAAAAB0eXBlAAAAAAcAAAAAAAAAcmVndWxh"
  ,"cgAIAAAAAAAAAGNvbnRlbnRzCAAAAAAAAABmb28gdGV4dAEAAAAAAAAAKQAAAAAAAAABAAAAAAAA"
  ,"ACkAAAAAAAAAAQAAAAAAAAApAAAAAAAAAAEAAAAAAAAAKQAAAAAAAAAFAAAAAAAAAGVudHJ5AAAA"
  ,"AQAAAAAAAAAoAAAAAAAAAAQAAAAAAAAAbmFtZQAAAAAHAAAAAAAAAGxpbmtmb28ABAAAAAAAAABu"
  ,"b2RlAAAAAAEAAAAAAAAAKAAAAAAAAAAEAAAAAAAAAHR5cGUAAAAABwAAAAAAAABzeW1saW5rAAYA"
  ,"AAAAAAAAdGFyZ2V0AAADAAAAAAAAAGZvbwAAAAAAAQAAAAAAAAApAAAAAAAAAAEAAAAAAAAAKQAA"
  ,"AAAAAAABAAAAAAAAACkAAAAAAAAA"
  ]

-- | Control testcase sizes (bytes) by env variable
getBigFileSize :: IO Int64
getBigFileSize = fromMaybe 5000000 . readMaybe <$> (getEnv "HNIX_BIG_FILE_SIZE" <|> pure "")


-- | Add a link to a FileSystemObject. This is useful
--   when creating Arbitrary FileSystemObjects. It
--   isn't implemented yet
mkLink
  :: FilePath -- ^ Target
  -> FilePath -- ^ Link
  -> FileSystemObject -- ^ FileSystemObject to add link to
  -> FileSystemObject
mkLink = undefined -- TODO

mkBigFile :: FilePath -> IO ()
mkBigFile path = do
  fsize <- getBigFileSize
  BSL.writeFile path (BSL.take fsize $ BSL.cycle "Lorem ipsum")


-- | Construct FilePathPart from Text by checking that there
--   are no '/' or '\\NUL' characters
filePathPart :: BSC.ByteString -> Maybe FilePathPart
filePathPart p = if BSC.any (`elem` ['/', '\NUL']) p then Nothing else Just $ FilePathPart p

newtype Nar = Nar { narFile :: FileSystemObject }
    deriving (Eq, Show, Generic)

-- | A FileSystemObject (FSO) is an anonymous entity that can be NAR archived
data FileSystemObject =
    Regular Nar.IsExecutable Int64 BSL.ByteString
    -- ^ Reguar file, with its executable state, size (bytes) and contents
  | Directory (Map.Map FilePathPart FileSystemObject)
    -- ^ Directory with mapping of filenames to sub-FSOs
  | SymLink T.Text
    -- ^ Symbolic link target
  deriving (Eq, Show)

-- | A valid filename or directory name
newtype FilePathPart = FilePathPart { unFilePathPart :: BSC.ByteString }
  deriving (Eq, Ord, Show)

instance Binary Nar where
  get = getNar
  put = putNar

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
            <$> elements [Nar.NonExecutable, Nar.Executable]
            <*> pure (fromIntegral fSize)
            <*> oneof  [
                  fmap (BSL.take fSize . BSL.cycle . BSL.pack . getNonEmpty) arbitrary , -- Binary File
                  fmap (BSL.take fSize . BSL.cycle . BSLC.pack . getNonEmpty) arbitrary   -- ASCII  File
                  ]

        arbName :: Gen FilePathPart
        arbName = fmap (FilePathPart . BS.pack . fmap (fromIntegral . fromEnum)) $ do
          Positive n <- arbitrary
          replicateM n (elements $ ['a'..'z'] <> ['0'..'9'])

        arbDirectory :: Int -> Gen FileSystemObject
        arbDirectory n = fmap (Directory . Map.fromList) $ replicateM n $ do
          nm <- arbName
          f <- oneof [arbFile, arbDirectory (n `div` 2)]
          pure (nm,f)

------------------------------------------------------------------------------
-- | Serialize Nar to lazy ByteString
putNar :: Nar -> Put
putNar (Nar file) = header <> parens (putFile file)
    where

        header   = str "nix-archive-1"

        putFile (Regular isExec fSize contents) =
               strs ["type", "regular"]
            >> (if isExec == Nar.Executable
               then strs ["executable", ""]
               else pass)
            >> putContents fSize contents

        putFile (SymLink target) =
               strs ["type", "symlink", "target", fromStrict $ encodeUtf8 target]

        -- toList sorts the entries by FilePathPart before serializing
        putFile (Directory entries) =
               strs ["type", "directory"]
            <> mapM_ putEntry (Map.toList entries)

        putEntry (FilePathPart name, fso) = do
            str "entry"
            parens $ do
              str "name"
              str (fromStrict name)
              str "node"
              parens (putFile fso)

        parens m = str "(" >> m >> str ")"

        -- Do not use this for file contents
        str :: BSL.ByteString -> Put
        str t = let len = BSL.length t
            in int len <> pad len t

        putContents :: Int64 -> BSL.ByteString -> Put
        putContents fSize bs = str "contents" <> int fSize <> pad fSize bs

        int :: Integral a => a -> Put
        int n = putInt64le $ fromIntegral n

        pad :: Int64 -> BSL.ByteString -> Put
        pad strSize bs = do
          putLazyByteString bs
          putLazyByteString (BSL.replicate (padLen strSize) 0)

        strs :: [BSL.ByteString] -> Put
        strs = mapM_ str

-- | Distance to the next multiple of 8
padLen :: Int64 -> Int64
padLen n = (8 - n) `mod` 8


------------------------------------------------------------------------------
-- | Deserialize a Nar from lazy ByteString
getNar :: Get Nar
getNar = fmap Nar $ header >> parens getFile
    where

      header   = assertStr "nix-archive-1"

      -- Fetch a FileSystemObject
      getFile = getRegularFile <|> getDirectory <|> getSymLink

      getRegularFile = do
          assertStr_ "type"
          assertStr_ "regular"
          mExecutable <- optional $ Nar.Executable <$ (assertStr "executable"
                                                       >> assertStr "")
          assertStr_ "contents"
          (fSize, contents) <- sizedStr
          pure $ Regular (fromMaybe Nar.NonExecutable mExecutable) fSize contents

      getDirectory = do
          assertStr_ "type"
          assertStr_ "directory"
          fs <- many getEntry
          pure $ Directory (Map.fromList fs)

      getSymLink = do
          assertStr_ "type"
          assertStr_ "symlink"
          assertStr_ "target"
          fmap (SymLink . decodeUtf8) str

      getEntry = do
          assertStr_ "entry"
          parens $ do
              assertStr_ "name"
              name <- str
              assertStr_ "node"
              file <- parens getFile
              maybe (fail $ "Bad FilePathPart: " <> show name)
                    (pure . (,file))
                    (filePathPart $ toStrict name)

      -- Fetch a length-prefixed, null-padded string
      str = fmap snd sizedStr

      sizedStr = do
          n <- getInt64le
          s <- getLazyByteString n
          _ <- getByteString . fromIntegral $ padLen n
          pure (n,s)

      parens m = assertStr "(" *> m <* assertStr ")"

      assertStr_ = void . assertStr
      assertStr s = do
          s' <- str
          if s == s'
              then pure s
              else fail "No"
