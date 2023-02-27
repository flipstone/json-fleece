{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesBaseResponse
  ( ComicSeriesBaseResponse(..)
  , comicSeriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ComicSeriesBaseResponse = ComicSeriesBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ Base comic series, returned in search results
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

comicSeriesBaseResponseSchema :: FC.Fleece schema => schema ComicSeriesBaseResponse
comicSeriesBaseResponseSchema =
  FC.object $
    FC.constructor ComicSeriesBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optional "page" page responsePageSchema