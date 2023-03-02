{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripBaseResponse
  ( ComicStripBaseResponse(..)
  , comicStripBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ComicStripBase as ComicStripBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data ComicStripBaseResponse = ComicStripBaseResponse
  { comicStrips :: Maybe [ComicStripBase.ComicStripBase] -- ^ Base comic strip, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

comicStripBaseResponseSchema :: FC.Fleece schema => schema ComicStripBaseResponse
comicStripBaseResponseSchema =
  FC.object $
    FC.constructor ComicStripBaseResponse
      #+ FC.optional "comicStrips" comicStrips (FC.list ComicStripBase.comicStripBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema