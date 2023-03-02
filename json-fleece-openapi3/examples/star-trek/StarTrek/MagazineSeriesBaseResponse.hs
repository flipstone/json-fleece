{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesBaseResponse
  ( MagazineSeriesBaseResponse(..)
  , magazineSeriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.MagazineSeriesBase as MagazineSeriesBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data MagazineSeriesBaseResponse = MagazineSeriesBaseResponse
  { magazineSeries :: Maybe [MagazineSeriesBase.MagazineSeriesBase] -- ^ Base magazine series, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

magazineSeriesBaseResponseSchema :: FC.Fleece schema => schema MagazineSeriesBaseResponse
magazineSeriesBaseResponseSchema =
  FC.object $
    FC.constructor MagazineSeriesBaseResponse
      #+ FC.optional "magazineSeries" magazineSeries (FC.list MagazineSeriesBase.magazineSeriesBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema