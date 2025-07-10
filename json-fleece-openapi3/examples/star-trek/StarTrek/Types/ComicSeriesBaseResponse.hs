{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesBaseResponse
  ( ComicSeriesBaseResponse(..)
  , comicSeriesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicSeriesBase as ComicSeriesBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data ComicSeriesBaseResponse = ComicSeriesBaseResponse
  { comicSeries :: Maybe [ComicSeriesBase.ComicSeriesBase] -- ^ Base comic series, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

comicSeriesBaseResponseSchema :: FC.Fleece schema => schema ComicSeriesBaseResponse
comicSeriesBaseResponseSchema =
  FC.object $
    FC.constructor ComicSeriesBaseResponse
      #+ FC.optional "comicSeries" comicSeries (FC.list ComicSeriesBase.comicSeriesBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema