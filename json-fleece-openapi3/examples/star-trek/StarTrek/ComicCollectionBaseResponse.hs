{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionBaseResponse
  ( ComicCollectionBaseResponse(..)
  , comicCollectionBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ComicCollectionBase as ComicCollectionBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data ComicCollectionBaseResponse = ComicCollectionBaseResponse
  { comicCollections :: Maybe [ComicCollectionBase.ComicCollectionBase] -- ^ Base comic collection, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

comicCollectionBaseResponseSchema :: FC.Fleece schema => schema ComicCollectionBaseResponse
comicCollectionBaseResponseSchema =
  FC.object $
    FC.constructor ComicCollectionBaseResponse
      #+ FC.optional "comicCollections" comicCollections (FC.list ComicCollectionBase.comicCollectionBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema