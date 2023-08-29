{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesBaseResponse
  ( BookSeriesBaseResponse(..)
  , bookSeriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.BookSeriesBase as BookSeriesBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data BookSeriesBaseResponse = BookSeriesBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , bookSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  }
  deriving (Eq, Show)

bookSeriesBaseResponseSchema :: FC.Fleece schema => schema BookSeriesBaseResponse
bookSeriesBaseResponseSchema =
  FC.object $
    FC.constructor BookSeriesBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "bookSeries" bookSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)