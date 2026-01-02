{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesFullResponse
  ( ComicSeriesFullResponse(..)
  , comicSeriesFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicSeriesFull as ComicSeriesFull

data ComicSeriesFullResponse = ComicSeriesFullResponse
  { comicSeries :: Maybe ComicSeriesFull.ComicSeriesFull -- ^ Full comic series, returned when queried using UID
  }
  deriving (Eq, Show)

comicSeriesFullResponseSchema :: FC.Fleece t => FC.Schema t ComicSeriesFullResponse
comicSeriesFullResponseSchema =
  FC.object $
    FC.constructor ComicSeriesFullResponse
      #+ FC.optional "comicSeries" comicSeries ComicSeriesFull.comicSeriesFullSchema