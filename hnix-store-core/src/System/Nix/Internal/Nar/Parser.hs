-- | A streaming parser for the NAR format

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module System.Nix.Internal.Nar.Parser
  ( runParser
  , parseNar
  , testParser
  , testParser'
  )
where

import qualified Algebra.Graph                   as Graph
import qualified Algebra.Graph.ToGraph           as Graph
import qualified Control.Concurrent              as Concurrent
import qualified Control.Exception.Lifted        as Exception.Lifted
import           Control.Monad                    ( forM
                                                  , when
                                                  , forM_
                                                  )
import qualified Control.Monad.Except            as Except
import qualified Control.Monad.Fail              as Fail
import qualified Control.Monad.IO.Class          as IO
import qualified Control.Monad.Reader            as Reader
import qualified Control.Monad.State             as State
import qualified Control.Monad.Trans             as Trans
import qualified Control.Monad.Trans.Control     as Base
import           Data.ByteString                  ( ByteString )
import qualified Data.ByteString                 as Bytes
import           Data.Bool                        ( bool )
import qualified Data.Either                     as Either
import           Data.Int                         ( Int64 )
import qualified Data.IORef                      as IORef
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import           Data.Maybe                       ( catMaybes )
import qualified Data.Serialize                  as Serialize
import           Data.Text                        ( Text )
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import qualified System.Directory                as Directory
import           System.FilePath                 as FilePath
import qualified System.IO                       as IO

import qualified System.Nix.Internal.Nar.Effects as Nar


-- | NarParser is a monad for parsing a Nar file as a byte stream
--   and reconstructing the file system objects inside
--   See the definitions of @NarEffects@ for a description
--   of the actions the parser can take, and @ParserState@ for the
--   internals of the parser
newtype NarParser m a = NarParser
  { runNarParser ::
      State.StateT
        ParserState
        (Except.ExceptT
          String
          (Reader.ReaderT
            (Nar.NarEffects m)
            m
          )
        )
        a
  }
  deriving ( Functor, Applicative, Monad, Fail.MonadFail
           , Trans.MonadIO, State.MonadState ParserState
           , Except.MonadError String
           , Reader.MonadReader (Nar.NarEffects m)
           )

-- | Run a @NarParser@ over a byte stream
--   This is suitable for testing the top-level NAR parser, or any of the
--   smaller utilities parsers, if you have bytes appropriate for them
runParser
  :: forall m a
   . (IO.MonadIO m, Base.MonadBaseControl IO m)
  => Nar.NarEffects m
     -- ^ Provide the effects set, usually @narEffectsIO@
  -> NarParser m a
     -- ^ A parser to run, such as @parseNar@
  -> IO.Handle
     -- ^ A handle the stream containg the NAR. It should already be
     --   open and in @IO.ReadMode@
  -> FilePath
     -- ^ The root file system object to be created by the NAR
  -> m (Either String a)
runParser effs (NarParser action) h target = do
  unpackResult <-
    Reader.runReaderT (Except.runExceptT $ State.evalStateT action state0) effs
      `Exception.Lifted.catch` exceptionHandler
  when (Either.isLeft unpackResult) cleanup
  pure unpackResult

 where
  state0 :: ParserState
  state0 =
    ParserState
      { tokenStack     = []
      , handle         = h
      , directoryStack = [target]
      , links          = []
      }

  exceptionHandler :: Exception.Lifted.SomeException -> m (Either String a)
  exceptionHandler e =
    pure $ Left $ "Exception while unpacking NAR file: " <> show e

  cleanup :: m ()
  cleanup =
    (\ef trg -> do
      isDir <- Nar.narIsDir ef trg
      bool
        (Nar.narDeleteFile ef trg)
        (Nar.narDeleteDir ef trg)
        isDir
    ) effs target


instance Trans.MonadTrans NarParser where
  lift act = NarParser $ (Trans.lift . Trans.lift . Trans.lift) act


data ParserState = ParserState
  { tokenStack     :: ![Text]
    -- ^ The parser can push tokens (words or punctuation)
    --   onto this stack. We use this for a very limited backtracking
    --   where the Nar format requires it
  , directoryStack :: ![String]
    -- ^ The parser knows the name of the current FSO it's targeting,
    --   and the relative directory path leading there
  , handle         :: IO.Handle
    -- ^ Handle of the input byte stream
  , links          :: [LinkInfo]
    -- ^ Unlike with files and directories, we collect symlinks
    --   from the NAR on
  }


------------------------------------------------------------------------------
-- * Parsers for NAR components

-- | Parse a NAR byte string, producing @()@.
--   Parsing a NAR is mostly used for its side-effect: producing
--   the file system objects packed in the NAR. That's why we pure @()@
parseNar :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m ()
parseNar = do
  expectStr "nix-archive-1"
  parens parseFSO
  createLinks


parseFSO :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m ()
parseFSO = do
  expectStr "type"
  matchStr
    [ ("symlink"  , parseSymlink  )
    , ("regular"  , parseFile     )
    , ("directory", parseDirectory)
    ]


-- | Parse a symlink from a NAR, storing the link details in the parser state
--   We remember links rather than immediately creating file system objects
--   from them, because we might encounter a link in the NAR before we
--   encountered its target, and in this case, creating the link will fail
--   The final step of creating links is handle by @createLinks@
parseSymlink :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m ()
parseSymlink = do
  expectStr "target"
  target      <- parseStr
  (dir, file) <- currentDirectoryAndFile
  pushLink $
    LinkInfo
      { linkTarget = Text.unpack target
      , linkFile   = file
      , linkPWD    = dir
      }
 where
  currentDirectoryAndFile :: Monad m => NarParser m (FilePath, FilePath)
  currentDirectoryAndFile = do
    dirStack <- State.gets directoryStack
    pure (List.foldr1 (</>) (List.reverse $ drop 1 dirStack), head dirStack)


-- | Internal data type representing symlinks encountered in the NAR
data LinkInfo = LinkInfo
  { linkTarget :: String
    -- ^ path to the symlink target, relative to the root of the unpacking NAR
  , linkFile   :: String
    -- ^ file name of the link being created
  , linkPWD    :: String
    -- ^ directory in which to create the link (relative to unpacking root)
  }
  deriving Show


-- | When the NAR includes a file, we read from the NAR handle in chunks and
--   write the target in chunks. This lets us avoid reading the full contents
--   of the encoded file into memory
parseFile :: forall m . (IO.MonadIO m, Fail.MonadFail m) => NarParser m ()
parseFile = do

  s <- parseStr
  when (s `notElem` ["executable", "contents"]) $
    Fail.fail
      $ "Parser found " <> show s
      <> " when expecting element from "
      <> (show :: [String] -> String) ["executable", "contents"]
  when (s == "executable") $ do
    expectStr ""
    expectStr "contents"

  fSize        <- parseLength

  -- Set up for defining `getChunk`
  narHandle    <- State.gets handle
  bytesLeftVar <- IO.liftIO $ IORef.newIORef fSize

  let
    -- getChunk tracks the number of total bytes we still need to get from the
    -- file (starting at the file size, and decrementing by the size of the
    -- chunk we read)
    getChunk :: m (Maybe ByteString)
    getChunk = do
      bytesLeft <- IO.liftIO $ IORef.readIORef bytesLeftVar
      if bytesLeft == 0
        then pure Nothing
        else do
          chunk <- IO.liftIO $ Bytes.hGetSome narHandle $ fromIntegral $ min 10000 bytesLeft
          when (Bytes.null chunk) (Fail.fail "ZERO BYTES")
          IO.liftIO $ IORef.modifyIORef bytesLeftVar $ \n -> n - fromIntegral (Bytes.length chunk)

          -- This short pause is necessary for letting the garbage collector
          -- clean up chunks from previous runs. Without it, heap memory usage can
          -- quickly spike
          IO.liftIO $ Concurrent.threadDelay 10
          pure $ Just chunk

  target     <- currentFile
  streamFile <- Reader.asks Nar.narStreamFile
  Trans.lift (streamFile target getChunk)

  when (s == "executable") $ do
    effs :: Nar.NarEffects m <- Reader.ask
    Trans.lift $ do
      p <- Nar.narGetPerms effs target
      Nar.narSetPerms effs target (p { Directory.executable = True })

  expectRawString (Bytes.replicate (padLen $ fromIntegral fSize) 0)


-- | Parse a NAR encoded directory, being careful not to hold onto file
--   handles for target files longer than needed
parseDirectory :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m ()
parseDirectory = do
  createDirectory <- Reader.asks Nar.narCreateDir
  target          <- currentFile
  Trans.lift $ createDirectory target
  parseEntryOrFinish

 where

  parseEntryOrFinish :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m ()
  parseEntryOrFinish =
    -- If we reach a ")", we finished the directory's entries, and we have
    -- to put ")" back into the stream, because the outer call to @parens@
    -- expects to consume it.
    -- Otherwise, parse an entry as a fresh file system object
    matchStr
      [ ( ")"   , pushStr ")" )
      , ("entry", parseEntry  )
      ]

  parseEntry :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m ()
  parseEntry = do
    parens $ do
      expectStr "name"
      fName <- parseStr
      pushFileName (Text.unpack fName)
      expectStr "node"
      parens parseFSO
      popFileName
    parseEntryOrFinish



------------------------------------------------------------------------------
-- * Utility parsers


-- | Short strings guiding the NAR parsing are prefixed with their
--   length, then encoded in ASCII, and padded to 8 bytes. @parseStr@
--   captures this logic
parseStr :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m Text
parseStr = do
  cachedStr <- popStr
  case cachedStr of
    Just str -> pure str
    Nothing  -> do
      len      <- parseLength
      strBytes <- consume $ fromIntegral len
      expectRawString
        (Bytes.replicate (fromIntegral $ padLen $ fromIntegral len) 0)
      pure $ Text.decodeUtf8 strBytes


-- | Get an Int64 describing the length of the upcoming string,
--   according to NAR's encoding of ints
parseLength :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m Int64
parseLength = do
  eightBytes <- consume 8
  either
    (\e -> Fail.fail $ "parseLength failed to decode int64: " <> e)
    pure
    (Serialize.runGet Serialize.getInt64le eightBytes)


-- | Consume a NAR string and assert that it matches an expectation
expectStr :: (IO.MonadIO m, Fail.MonadFail m) => Text -> NarParser m ()
expectStr expected = do
  actual <- parseStr
  when (actual /= expected) $
    Fail.fail $  "Expected " <> err expected <> ", got " <> err actual
 where
  err t =
    show $
      bool
        t
        (Text.take 10 t <> "...")
        (Text.length t > 10)


-- | Consume a raw string and assert that it equals some expectation.
--   This is usually used when consuming padding 0's
expectRawString
  :: (IO.MonadIO m, Fail.MonadFail m) => ByteString -> NarParser m ()
expectRawString expected = do
  actual <- consume $ Bytes.length expected
  when (actual /= expected)
    $  Fail.fail
    $  "Expected "
    <> err expected
    <> ", got "
    <> err actual
 where
  err bs =
    show $
      bool
        bs
        (Bytes.take 10 bs <> "...")
        (Bytes.length bs > 10)


-- | Consume a NAR string, and dispatch to a parser depending on which string
--   matched
matchStr
  :: (IO.MonadIO m, Fail.MonadFail m)
  => [(Text, NarParser m a)]
     -- ^ List of expected possible strings and the parsers they should run
  -> NarParser m a
matchStr parsers = do
  str <- parseStr
  case List.lookup str parsers of
    Just p -> p
    Nothing ->
      Fail.fail $ "Expected one of " <> show (fst <$> parsers) <> " found " <> show str


-- | Wrap any parser in NAR formatted parentheses
--   (a parenthesis is a NAR string, so it needs length encoding and padding)
parens :: (IO.MonadIO m, Fail.MonadFail m) => NarParser m a -> NarParser m a
parens act = do
  expectStr "("
  r <- act
  expectStr ")"
  pure r


-- | Sort links in the symlink stack according to their connectivity
--   (Targets must be created before the links that target them)
createLinks :: IO.MonadIO m => NarParser m ()
createLinks = do
  createLink  <- Reader.asks Nar.narCreateLink
  allLinks    <- State.gets links
  sortedLinks <- IO.liftIO $ sortLinksIO allLinks
  forM_ sortedLinks $ \li -> do
    pwd <- IO.liftIO Directory.getCurrentDirectory
    IO.liftIO $ Directory.setCurrentDirectory (linkPWD li)
    Trans.lift $ createLink (linkTarget li) (linkFile li)
    IO.liftIO $ Directory.setCurrentDirectory pwd

 where

  -- Convert every target and link file to a filepath relative
  -- to NAR root, then @Graph.topSort@ it, and map from the
  -- relative filepaths back to the original @LinkInfo@.
  -- Relative paths are needed for sorting, but @LinkInfo@s
  -- are needed for creating the link files
  sortLinksIO :: [LinkInfo] -> IO [LinkInfo]
  sortLinksIO ls = do
    linkLocations <- fmap Map.fromList $
      forM ls $ \li->
                  (,li) <$> Directory.canonicalizePath (linkFile li)
    canonicalLinks <- forM ls $ \l -> do
      targetAbsPath <- Directory.canonicalizePath
                        (linkPWD l </> linkTarget l)
      fileAbsPath   <- Directory.canonicalizePath
                        (linkFile l)
      pure (fileAbsPath, targetAbsPath)
    let linkGraph = Graph.edges canonicalLinks
    case Graph.topSort linkGraph of
      Left _            -> error "Symlinks form a loop"
      Right sortedNodes ->
        let
          sortedLinks = flip Map.lookup linkLocations <$> sortedNodes
        in
          pure $ catMaybes sortedLinks


------------------------------------------------------------------------------
-- * State manipulation

-- | Pull n bytes from the underlying handle, failing if fewer bytes
--   are available
consume
  :: (IO.MonadIO m, Fail.MonadFail m)
  => Int
  -> NarParser m ByteString
consume 0 = pure ""
consume n = do
  state0   <- State.get
  newBytes <- IO.liftIO $ Bytes.hGetSome (handle state0) (max 0 n)
  when (Bytes.length newBytes < n) $
    Fail.fail $
    "consume: Not enough bytes in handle. Wanted "
    <> show n <> " got " <> show (Bytes.length newBytes)
  pure newBytes


-- | Pop a string off the token stack
popStr :: Monad m => NarParser m (Maybe Text)
popStr = do
  s <- State.get
  case List.uncons (tokenStack s) of
    Nothing      -> pure Nothing
    Just (x, xs) -> do
      State.put $ s { tokenStack = xs }
      pure $ Just x


-- | Push a string onto the token stack
pushStr :: Monad m => Text -> NarParser m ()
pushStr str =
  State.modify $ \s -> -- s { loadedBytes = strBytes <> loadedBytes s }
    s { tokenStack = str : tokenStack s }


-- | Push a level onto the directory stack
pushFileName :: Monad m => FilePath -> NarParser m ()
pushFileName fName =
  State.modify (\s -> s { directoryStack = fName : directoryStack s })


-- | Go to the parent level in the directory stack
popFileName :: Monad m => NarParser m ()
popFileName =
  State.modify (\s -> s { directoryStack = List.drop 1 (directoryStack s )})


-- | Convert the current directory stack into a filepath by interspersing
--   the path components with "/"
currentFile :: Monad m => NarParser m FilePath
currentFile = do
  dirStack <- State.gets directoryStack
  pure $ List.foldr1 (</>) $ List.reverse dirStack


-- | Add a link to the collection of encountered symlinks
pushLink :: Monad m => LinkInfo -> NarParser m ()
pushLink linkInfo = State.modify (\s -> s { links = linkInfo : links s })


------------------------------------------------------------------------------
-- * Utilities

testParser :: (m ~ IO) => NarParser m a -> ByteString -> m (Either String a)
testParser p b = do
  Bytes.writeFile tmpFileName b
  IO.withFile tmpFileName IO.ReadMode $ \h ->
    runParser Nar.narEffectsIO p h tmpFileName
 where
  tmpFileName = "tmp"

testParser' :: (m ~ IO) => FilePath -> IO (Either String ())
testParser' fp =
  IO.withFile fp IO.ReadMode $ \h -> runParser Nar.narEffectsIO parseNar h "tmp"




-- | Distance to the next multiple of 8
padLen :: Int -> Int
padLen n = (8 - n) `mod` 8


dbgState :: IO.MonadIO m => NarParser m ()
dbgState = do
  s <- State.get
  IO.liftIO $ print (tokenStack s, directoryStack s)
