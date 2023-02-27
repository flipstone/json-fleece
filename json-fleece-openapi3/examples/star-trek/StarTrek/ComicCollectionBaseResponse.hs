{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionBaseResponse
  ( ComicCollectionBaseResponse(..)
  , comicCollectionBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicCollectionBase (ComicCollectionBase, comicCollectionBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ComicCollectionBaseResponse = ComicCollectionBaseResponse
  { comicCollections :: Maybe [ComicCollectionBase] -- ^ List of comic collections matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

comicCollectionBaseResponseSchema :: FC.Fleece schema => schema ComicCollectionBaseResponse
comicCollectionBaseResponseSchema =
  FC.object $
    FC.constructor ComicCollectionBaseResponse
      #+ FC.optional "comicCollections" comicCollections (FC.list comicCollectionBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema