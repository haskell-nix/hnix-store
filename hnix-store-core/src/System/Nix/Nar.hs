{-|
Description : Effects and functions for interacting with Nar files.
|-}

module System.Nix.Nar
  ( FileSystemObject(..)
  , IsExecutable(..)
  , Nar(..)
  , getNar
  , localPackNar
  , localUnpackNar
  , narEffectsIO
  , putNar
  , FilePathPart(..)
  , filePathPart
  ) where

import System.Nix.Internal.Nar
