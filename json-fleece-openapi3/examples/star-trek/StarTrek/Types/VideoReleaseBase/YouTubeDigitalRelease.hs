{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.YouTubeDigitalRelease
  ( YouTubeDigitalRelease(..)
  , youTubeDigitalReleaseSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype YouTubeDigitalRelease = YouTubeDigitalRelease Bool
  deriving (Show, Eq)

youTubeDigitalReleaseSchema :: FC.Fleece schema => schema YouTubeDigitalRelease
youTubeDigitalReleaseSchema =
  FC.coerceSchema FC.boolean