{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicCollectionBaseResponse
  ( ComicCollectionBaseResponse(..)
  , comicCollectionBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicCollectionBase as ComicCollectionBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data ComicCollectionBaseResponse = ComicCollectionBaseResponse
  { comicCollections :: Maybe [ComicCollectionBase.ComicCollectionBase] -- ^ Base comic collection, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

comicCollectionBaseResponseSchema :: FC.Fleece t => FC.Schema t ComicCollectionBaseResponse
comicCollectionBaseResponseSchema =
  FC.object $
    FC.constructor ComicCollectionBaseResponse
      #+ FC.optional "comicCollections" comicCollections (FC.list ComicCollectionBase.comicCollectionBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema