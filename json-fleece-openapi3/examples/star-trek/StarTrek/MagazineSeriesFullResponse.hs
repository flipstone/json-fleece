{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesFullResponse
  ( MagazineSeriesFullResponse(..)
  , magazineSeriesFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MagazineSeriesFull (MagazineSeriesFull, magazineSeriesFullSchema)

data MagazineSeriesFullResponse = MagazineSeriesFullResponse
  { magazineSeries :: Maybe MagazineSeriesFull -- ^ Full magazine series, returned when queried using UID
  }
  deriving (Eq, Show)

magazineSeriesFullResponseSchema :: FC.Fleece schema => schema MagazineSeriesFullResponse
magazineSeriesFullResponseSchema =
  FC.object $
    FC.constructor MagazineSeriesFullResponse
      #+ FC.optional "magazineSeries" magazineSeries magazineSeriesFullSchema