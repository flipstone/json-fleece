{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBaseResponse
  ( MovieBaseResponse(..)
  , movieBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MovieBase (MovieBase, movieBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MovieBaseResponse = MovieBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , movies :: Maybe [MovieBase] -- ^ Base movie, returned in search results
  }
  deriving (Eq, Show)

movieBaseResponseSchema :: FC.Fleece schema => schema MovieBaseResponse
movieBaseResponseSchema =
  FC.object $
    FC.constructor MovieBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "movies" movies (FC.list movieBaseSchema)