{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesBaseResponse
  ( BookSeriesBaseResponse(..)
  , bookSeriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.BookSeriesBase as BookSeriesBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data BookSeriesBaseResponse = BookSeriesBaseResponse
  { bookSeries :: Maybe [BookSeriesBase.BookSeriesBase] -- ^ Base book series, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

bookSeriesBaseResponseSchema :: FC.Fleece schema => schema BookSeriesBaseResponse
bookSeriesBaseResponseSchema =
  FC.object $
    FC.constructor BookSeriesBaseResponse
      #+ FC.optional "bookSeries" bookSeries (FC.list BookSeriesBase.bookSeriesBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema