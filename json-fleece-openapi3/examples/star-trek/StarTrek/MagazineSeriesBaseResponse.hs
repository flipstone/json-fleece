{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesBaseResponse
  ( MagazineSeriesBaseResponse(..)
  , magazineSeriesBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MagazineSeriesBase (MagazineSeriesBase, magazineSeriesBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MagazineSeriesBaseResponse = MagazineSeriesBaseResponse
  { magazineSeries :: Maybe [MagazineSeriesBase] -- ^ List of magazine series matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

magazineSeriesBaseResponseSchema :: FC.Fleece schema => schema MagazineSeriesBaseResponse
magazineSeriesBaseResponseSchema =
  FC.object $
    FC.constructor MagazineSeriesBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "magazineSeries" magazineSeries (FC.list magazineSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema