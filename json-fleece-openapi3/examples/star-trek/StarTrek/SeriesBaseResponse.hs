{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBaseResponse
  ( SeriesBaseResponse(..)
  , seriesBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.SeriesBase (SeriesBase, seriesBaseSchema)

data SeriesBaseResponse = SeriesBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , series :: Maybe [SeriesBase] -- ^ List of series matching given criteria
  }
  deriving (Eq, Show)

seriesBaseResponseSchema :: FC.Fleece schema => schema SeriesBaseResponse
seriesBaseResponseSchema =
  FC.object $
    FC.constructor SeriesBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "series" series (FC.list seriesBaseSchema)