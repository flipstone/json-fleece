{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesBaseResponse
  ( ComicSeriesBaseResponse(..)
  , comicSeriesBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicSeriesBase (ComicSeriesBase, comicSeriesBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ComicSeriesBaseResponse = ComicSeriesBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , comicSeries :: Maybe [ComicSeriesBase] -- ^ List of comic series matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

comicSeriesBaseResponseSchema :: FC.Fleece schema => schema ComicSeriesBaseResponse
comicSeriesBaseResponseSchema =
  FC.object $
    FC.constructor ComicSeriesBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicSeries" comicSeries (FC.list comicSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema