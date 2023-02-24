{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookSeriesBaseResponse
  ( BookSeriesBaseResponse(..)
  , bookSeriesBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.BookSeriesBase (BookSeriesBase, bookSeriesBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data BookSeriesBaseResponse = BookSeriesBaseResponse
  { bookSeries :: Maybe [BookSeriesBase] -- ^ List of book series matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

bookSeriesBaseResponseSchema :: FC.Fleece schema => schema BookSeriesBaseResponse
bookSeriesBaseResponseSchema =
  FC.object $
    FC.constructor BookSeriesBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "bookSeries" bookSeries (FC.list bookSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema