{-
Description : Generating and consuming NAR files
Maintainer  : Shea Levy <shea@shealevy.com>
-}

module System.Nix.Nar
  (
  -- * Encoding and Decoding NAR archives
    buildNarIO
  , unpackNarIO

  -- * Experimental
  , Nar.parseNar
  , Nar.testParser
  , Nar.testParser'

  -- * Filesystem capabilities used by NAR encoder/decoder
  , Nar.NarEffects(..)
  , Nar.narEffectsIO

  , Nar.NarOptions(..)
  , Nar.defaultNarOptions

  -- * Internal
  , Nar.streamNarIO
  , Nar.streamNarIOWithOptions
  , Nar.runParser
  , Nar.runParserWithOptions
  , Nar.dumpString
  , Nar.dumpPath

  -- * Type
  , Nar.NarSource
  ) where

import qualified Control.Concurrent                as Concurrent
import qualified Data.ByteString                   as BS
import qualified System.IO                         as IO

import qualified System.Nix.Nar.Effects   as Nar
import qualified System.Nix.Nar.Options   as Nar
import qualified System.Nix.Nar.Parser    as Nar
import qualified System.Nix.Nar.Streamer  as Nar

-- For a description of the NAR format, see Eelco's thesis
-- https://nixos.org/~eelco/pubs/phd-thesis.pdf

-- | Pack the filesystem object at @FilePath@ into a NAR and stream it into the
--   @IO.Handle@
--   The handle should aleady be open and in @WriteMode@.
buildNarIO
  :: Nar.NarEffects IO
  -> FilePath
  -> IO.Handle
  -> IO ()
buildNarIO effs basePath outHandle =
  Nar.streamNarIO
    effs
    basePath
    (\chunk -> BS.hPut outHandle chunk >> Concurrent.threadDelay 10)

-- | Read NAR formatted bytes from the @IO.Handle@ and unpack them into
--   file system object(s) at the supplied @FilePath@
unpackNarIO
  :: Nar.NarEffects IO
  -> IO.Handle
  -> FilePath
  -> IO (Either String ())
unpackNarIO effs = Nar.runParser effs Nar.parseNar
