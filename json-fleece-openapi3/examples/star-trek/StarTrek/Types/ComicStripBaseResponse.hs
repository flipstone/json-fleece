{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripBaseResponse
  ( ComicStripBaseResponse(..)
  , comicStripBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicStripBase as ComicStripBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data ComicStripBaseResponse = ComicStripBaseResponse
  { comicStrips :: Maybe [ComicStripBase.ComicStripBase] -- ^ Base comic strip, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

comicStripBaseResponseSchema :: FC.Fleece t => FC.Schema t ComicStripBaseResponse
comicStripBaseResponseSchema =
  FC.object $
    FC.constructor ComicStripBaseResponse
      #+ FC.optional "comicStrips" comicStrips (FC.list ComicStripBase.comicStripBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema