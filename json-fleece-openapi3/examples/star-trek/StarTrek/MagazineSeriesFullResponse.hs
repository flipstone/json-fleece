{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesFullResponse
  ( MagazineSeriesFullResponse(..)
  , magazineSeriesFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.MagazineSeriesFull as MagazineSeriesFull

data MagazineSeriesFullResponse = MagazineSeriesFullResponse
  { magazineSeries :: Maybe MagazineSeriesFull.MagazineSeriesFull -- ^ Full magazine series, returned when queried using UID
  }
  deriving (Eq, Show)

magazineSeriesFullResponseSchema :: FC.Fleece schema => schema MagazineSeriesFullResponse
magazineSeriesFullResponseSchema =
  FC.object $
    FC.constructor MagazineSeriesFullResponse
      #+ FC.optional "magazineSeries" magazineSeries MagazineSeriesFull.magazineSeriesFullSchema