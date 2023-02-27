{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesFullResponse
  ( ComicSeriesFullResponse(..)
  , comicSeriesFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicSeriesFull (ComicSeriesFull, comicSeriesFullSchema)

data ComicSeriesFullResponse = ComicSeriesFullResponse
  { comicSeries :: Maybe ComicSeriesFull -- ^ Full comic series, returned when queried using UID
  }
  deriving (Eq, Show)

comicSeriesFullResponseSchema :: FC.Fleece schema => schema ComicSeriesFullResponse
comicSeriesFullResponseSchema =
  FC.object $
    FC.constructor ComicSeriesFullResponse
      #+ FC.optional "comicSeries" comicSeries comicSeriesFullSchema