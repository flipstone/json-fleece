{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesFullResponse
  ( BookSeriesFullResponse(..)
  , bookSeriesFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookSeriesFull as BookSeriesFull

data BookSeriesFullResponse = BookSeriesFullResponse
  { bookSeries :: Maybe BookSeriesFull.BookSeriesFull -- ^ Full book series, returned when queried using UID
  }
  deriving (Eq, Show)

bookSeriesFullResponseSchema :: FC.Fleece schema => schema BookSeriesFullResponse
bookSeriesFullResponseSchema =
  FC.object $
    FC.constructor BookSeriesFullResponse
      #+ FC.optional "bookSeries" bookSeries BookSeriesFull.bookSeriesFullSchema