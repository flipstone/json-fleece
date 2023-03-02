{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBaseResponse
  ( SeriesBaseResponse(..)
  , seriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.SeriesBase as SeriesBase

data SeriesBaseResponse = SeriesBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , series :: Maybe [SeriesBase.SeriesBase] -- ^ Base series, returned in search results
  }
  deriving (Eq, Show)

seriesBaseResponseSchema :: FC.Fleece schema => schema SeriesBaseResponse
seriesBaseResponseSchema =
  FC.object $
    FC.constructor SeriesBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "series" series (FC.list SeriesBase.seriesBaseSchema)