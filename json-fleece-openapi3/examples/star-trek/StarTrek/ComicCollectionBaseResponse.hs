{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionBaseResponse
  ( ComicCollectionBaseResponse(..)
  , comicCollectionBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicCollections" comicCollections (FC.list comicCollectionBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema