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
  , putNar
  , FilePathPart(..)
  , filePathPart
  , NarEffects(..)
  ) where

import System.Nix.Internal.Nar
