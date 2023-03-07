{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFullResponse
  ( VideoReleaseFullResponse(..)
  , videoReleaseFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.VideoReleaseFull as VideoReleaseFull

data VideoReleaseFullResponse = VideoReleaseFullResponse
  { videoRelease :: Maybe VideoReleaseFull.VideoReleaseFull -- ^ Full video release, returned when queried using UID
  }
  deriving (Eq, Show)

videoReleaseFullResponseSchema :: FC.Fleece schema => schema VideoReleaseFullResponse
videoReleaseFullResponseSchema =
  FC.object $
    FC.constructor VideoReleaseFullResponse
      #+ FC.optional "videoRelease" videoRelease VideoReleaseFull.videoReleaseFullSchema