{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.VimeoDigitalRelease
  ( VimeoDigitalRelease(..)
  , vimeoDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VimeoDigitalRelease = VimeoDigitalRelease Bool
  deriving (Show, Eq)

vimeoDigitalReleaseSchema :: FC.Fleece t => FC.Schema t VimeoDigitalRelease
vimeoDigitalReleaseSchema =
  FC.coerceSchema FC.boolean