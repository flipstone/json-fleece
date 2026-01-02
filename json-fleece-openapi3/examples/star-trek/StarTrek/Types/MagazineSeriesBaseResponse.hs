{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineSeriesBaseResponse
  ( MagazineSeriesBaseResponse(..)
  , magazineSeriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MagazineSeriesBase as MagazineSeriesBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data MagazineSeriesBaseResponse = MagazineSeriesBaseResponse
  { magazineSeries :: Maybe [MagazineSeriesBase.MagazineSeriesBase] -- ^ Base magazine series, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

magazineSeriesBaseResponseSchema :: FC.Fleece t => FC.Schema t MagazineSeriesBaseResponse
magazineSeriesBaseResponseSchema =
  FC.object $
    FC.constructor MagazineSeriesBaseResponse
      #+ FC.optional "magazineSeries" magazineSeries (FC.list MagazineSeriesBase.magazineSeriesBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema