{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFullResponse
  ( VideoReleaseFullResponse(..)
  , videoReleaseFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.VideoReleaseFull (VideoReleaseFull, videoReleaseFullSchema)

data VideoReleaseFullResponse = VideoReleaseFullResponse
  { videoRelease :: Maybe VideoReleaseFull -- ^ Full video release, returned when queried using UID
  }
  deriving (Eq, Show)

videoReleaseFullResponseSchema :: FC.Fleece schema => schema VideoReleaseFullResponse
videoReleaseFullResponseSchema =
  FC.object $
    FC.constructor VideoReleaseFullResponse
      #+ FC.optional "videoRelease" videoRelease videoReleaseFullSchema