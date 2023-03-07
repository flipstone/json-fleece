{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoGameBaseResponse
  ( VideoGameBaseResponse(..)
  , videoGameBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.VideoGameBase as VideoGameBase

data VideoGameBaseResponse = VideoGameBaseResponse
  { videoGames :: Maybe [VideoGameBase.VideoGameBase] -- ^ Base video game, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

videoGameBaseResponseSchema :: FC.Fleece schema => schema VideoGameBaseResponse
videoGameBaseResponseSchema =
  FC.object $
    FC.constructor VideoGameBaseResponse
      #+ FC.optional "videoGames" videoGames (FC.list VideoGameBase.videoGameBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema