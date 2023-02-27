{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesFullResponse
  ( SeriesFullResponse(..)
  , seriesFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SeriesFull (SeriesFull, seriesFullSchema)

data SeriesFullResponse = SeriesFullResponse
  { series :: Maybe SeriesFull -- ^ Full series, returned when queried using UID
  }
  deriving (Eq, Show)

seriesFullResponseSchema :: FC.Fleece schema => schema SeriesFullResponse
seriesFullResponseSchema =
  FC.object $
    FC.constructor SeriesFullResponse
      #+ FC.optional "series" series seriesFullSchema