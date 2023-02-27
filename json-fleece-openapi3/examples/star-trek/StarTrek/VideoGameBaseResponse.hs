{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoGameBaseResponse
  ( VideoGameBaseResponse(..)
  , videoGameBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.VideoGameBase (VideoGameBase, videoGameBaseSchema)

data VideoGameBaseResponse = VideoGameBaseResponse
  { videoGames :: Maybe [VideoGameBase] -- ^ Base video game, returned in search results
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

videoGameBaseResponseSchema :: FC.Fleece schema => schema VideoGameBaseResponse
videoGameBaseResponseSchema =
  FC.object $
    FC.constructor VideoGameBaseResponse
      #+ FC.optional "videoGames" videoGames (FC.list videoGameBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema