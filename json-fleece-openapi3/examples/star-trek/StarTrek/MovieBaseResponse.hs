{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBaseResponse
  ( MovieBaseResponse(..)
  , movieBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.MovieBase as MovieBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data MovieBaseResponse = MovieBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , movies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  }
  deriving (Eq, Show)

movieBaseResponseSchema :: FC.Fleece schema => schema MovieBaseResponse
movieBaseResponseSchema =
  FC.object $
    FC.constructor MovieBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "movies" movies (FC.list MovieBase.movieBaseSchema)