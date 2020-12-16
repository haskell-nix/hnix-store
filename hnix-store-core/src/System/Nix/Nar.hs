{-
Description : Generating and consuming NAR files
Maintainer  : Shea Levy <shea@shealevy.com>
-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module System.Nix.Nar (

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

  -- * Internal
  , Nar.streamNarIO
  , Nar.runParser
  ) where

import qualified Control.Concurrent     as Concurrent
import qualified Data.ByteString        as BS
import qualified System.IO              as IO

import qualified System.Nix.Internal.Nar.Effects  as Nar
import qualified System.Nix.Internal.Nar.Parser   as Nar
import qualified System.Nix.Internal.Nar.Streamer as Nar


-- For a description of the NAR format, see Eelco's thesis
-- https://nixos.org/%7Eeelco/pubs/phd-thesis.pdf


-- | Pack the filesystem object at @FilePath@ into a NAR and stream it into the
--   @IO.Handle@
--   The handle should aleady be open and in @IO.WriteMode@.
buildNarIO
  :: Nar.NarEffects IO
  -> FilePath
  -> IO.Handle
  -> IO ()
buildNarIO effs basePath outHandle = do
  Nar.streamNarIO (\chunk -> BS.hPut outHandle chunk >> Concurrent.threadDelay 10) effs basePath


-- | Read NAR formatted bytes from the @IO.Handle@ and unpack them into
--   file system object(s) at the supplied @FilePath@
unpackNarIO
  :: Nar.NarEffects IO
  -> IO.Handle
  -> FilePath
  -> IO (Either String ())
unpackNarIO effs narHandle outputFile = do
  Nar.runParser effs Nar.parseNar narHandle outputFile
