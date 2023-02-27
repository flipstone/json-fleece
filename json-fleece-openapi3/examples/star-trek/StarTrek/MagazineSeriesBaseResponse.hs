{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesBaseResponse
  ( MagazineSeriesBaseResponse(..)
  , magazineSeriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MagazineSeriesBase (MagazineSeriesBase, magazineSeriesBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MagazineSeriesBaseResponse = MagazineSeriesBaseResponse
  { magazineSeries :: Maybe [MagazineSeriesBase] -- ^ Base magazine series, returned in search results
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

magazineSeriesBaseResponseSchema :: FC.Fleece schema => schema MagazineSeriesBaseResponse
magazineSeriesBaseResponseSchema =
  FC.object $
    FC.constructor MagazineSeriesBaseResponse
      #+ FC.optional "magazineSeries" magazineSeries (FC.list magazineSeriesBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema