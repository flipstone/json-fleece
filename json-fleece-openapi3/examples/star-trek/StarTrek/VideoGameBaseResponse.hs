{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameBaseResponse
  ( VideoGameBaseResponse(..)
  , videoGameBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.VideoGameBase (VideoGameBase, videoGameBaseSchema)

data VideoGameBaseResponse = VideoGameBaseResponse
  { videoGames :: Maybe [VideoGameBase] -- ^ List of video games matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

videoGameBaseResponseSchema :: FC.Fleece schema => schema VideoGameBaseResponse
videoGameBaseResponseSchema =
  FC.object $
    FC.constructor VideoGameBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "videoGames" videoGames (FC.list videoGameBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema