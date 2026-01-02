{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.YouTubeDigitalRelease
  ( YouTubeDigitalRelease(..)
  , youTubeDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype YouTubeDigitalRelease = YouTubeDigitalRelease Bool
  deriving (Show, Eq)

youTubeDigitalReleaseSchema :: FC.Fleece t => FC.Schema t YouTubeDigitalRelease
youTubeDigitalReleaseSchema =
  FC.coerceSchema FC.boolean