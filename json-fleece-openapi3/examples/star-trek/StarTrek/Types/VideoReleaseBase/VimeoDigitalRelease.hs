{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.VimeoDigitalRelease
  ( VimeoDigitalRelease(..)
  , vimeoDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VimeoDigitalRelease = VimeoDigitalRelease Bool
  deriving (Show, Eq)

vimeoDigitalReleaseSchema :: FC.Fleece schema => schema VimeoDigitalRelease
vimeoDigitalReleaseSchema =
  FC.coerceSchema FC.boolean