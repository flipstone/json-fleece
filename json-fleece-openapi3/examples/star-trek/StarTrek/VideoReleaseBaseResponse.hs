{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBaseResponse
  ( VideoReleaseBaseResponse(..)
  , videoReleaseBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.VideoReleaseBase (VideoReleaseBase, videoReleaseBaseSchema)

data VideoReleaseBaseResponse = VideoReleaseBaseResponse
  { videoReleases :: Maybe [VideoReleaseBase] -- ^ Base video release, returned in search results
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

videoReleaseBaseResponseSchema :: FC.Fleece schema => schema VideoReleaseBaseResponse
videoReleaseBaseResponseSchema =
  FC.object $
    FC.constructor VideoReleaseBaseResponse
      #+ FC.optional "videoReleases" videoReleases (FC.list videoReleaseBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema