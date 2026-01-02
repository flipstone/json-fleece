{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieBaseResponse
  ( MovieBaseResponse(..)
  , movieBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MovieBase as MovieBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data MovieBaseResponse = MovieBaseResponse
  { movies :: Maybe [MovieBase.MovieBase] -- ^ Base movie, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

movieBaseResponseSchema :: FC.Fleece t => FC.Schema t MovieBaseResponse
movieBaseResponseSchema =
  FC.object $
    FC.constructor MovieBaseResponse
      #+ FC.optional "movies" movies (FC.list MovieBase.movieBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema