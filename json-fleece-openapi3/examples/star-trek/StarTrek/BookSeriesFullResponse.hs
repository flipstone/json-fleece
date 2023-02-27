{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesFullResponse
  ( BookSeriesFullResponse(..)
  , bookSeriesFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookSeriesFull (BookSeriesFull, bookSeriesFullSchema)

data BookSeriesFullResponse = BookSeriesFullResponse
  { bookSeries :: Maybe BookSeriesFull -- ^ Full book series, returned when queried using UID
  }
  deriving (Eq, Show)

bookSeriesFullResponseSchema :: FC.Fleece schema => schema BookSeriesFullResponse
bookSeriesFullResponseSchema =
  FC.object $
    FC.constructor BookSeriesFullResponse
      #+ FC.optional "bookSeries" bookSeries bookSeriesFullSchema