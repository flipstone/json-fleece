{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicCollectionFullResponse
  ( ComicCollectionFullResponse(..)
  , comicCollectionFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicCollectionFull as ComicCollectionFull

data ComicCollectionFullResponse = ComicCollectionFullResponse
  { comicCollection :: Maybe ComicCollectionFull.ComicCollectionFull -- ^ Full comic collection, returned when queried using UID
  }
  deriving (Eq, Show)

comicCollectionFullResponseSchema :: FC.Fleece schema => schema ComicCollectionFullResponse
comicCollectionFullResponseSchema =
  FC.object $
    FC.constructor ComicCollectionFullResponse
      #+ FC.optional "comicCollection" comicCollection ComicCollectionFull.comicCollectionFullSchema