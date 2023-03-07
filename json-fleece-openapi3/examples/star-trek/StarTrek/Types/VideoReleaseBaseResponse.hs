{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBaseResponse
  ( VideoReleaseBaseResponse(..)
  , videoReleaseBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.VideoReleaseBase as VideoReleaseBase

data VideoReleaseBaseResponse = VideoReleaseBaseResponse
  { videoReleases :: Maybe [VideoReleaseBase.VideoReleaseBase] -- ^ Base video release, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

videoReleaseBaseResponseSchema :: FC.Fleece schema => schema VideoReleaseBaseResponse
videoReleaseBaseResponseSchema =
  FC.object $
    FC.constructor VideoReleaseBaseResponse
      #+ FC.optional "videoReleases" videoReleases (FC.list VideoReleaseBase.videoReleaseBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema