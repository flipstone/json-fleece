{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesFullResponse
  ( SeriesFullResponse(..)
  , seriesFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SeriesFull as SeriesFull

data SeriesFullResponse = SeriesFullResponse
  { series :: Maybe SeriesFull.SeriesFull -- ^ Full series, returned when queried using UID
  }
  deriving (Eq, Show)

seriesFullResponseSchema :: FC.Fleece schema => schema SeriesFullResponse
seriesFullResponseSchema =
  FC.object $
    FC.constructor SeriesFullResponse
      #+ FC.optional "series" series SeriesFull.seriesFullSchema