{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBaseResponse
  ( SeriesBaseResponse(..)
  , seriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
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
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "series" series (FC.list seriesBaseSchema)