{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionFullResponse
  ( ComicCollectionFullResponse(..)
  , comicCollectionFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicCollectionFull (ComicCollectionFull, comicCollectionFullSchema)

data ComicCollectionFullResponse = ComicCollectionFullResponse
  { comicCollection :: Maybe ComicCollectionFull -- ^ Full comic collection, returned when queried using UID
  }
  deriving (Eq, Show)

comicCollectionFullResponseSchema :: FC.Fleece schema => schema ComicCollectionFullResponse
comicCollectionFullResponseSchema =
  FC.object $
    FC.constructor ComicCollectionFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "comicCollection" comicCollection comicCollectionFullSchema