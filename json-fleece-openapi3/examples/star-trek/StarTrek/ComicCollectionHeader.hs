{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionHeader
  ( ComicCollectionHeader(..)
  , comicCollectionHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.ComicCollectionHeader.Title (Title, titleSchema)
import StarTrek.ComicCollectionHeader.Uid (Uid, uidSchema)

data ComicCollectionHeader = ComicCollectionHeader
  { uid :: Uid -- ^ Comic collection unique ID
  , title :: Title -- ^ Comic collection title
  }
  deriving (Eq, Show)

comicCollectionHeaderSchema :: FC.Fleece schema => schema ComicCollectionHeader
comicCollectionHeaderSchema =
  FC.object $
    FC.constructor ComicCollectionHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema