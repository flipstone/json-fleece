{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionHeader
  ( ComicCollectionHeader(..)
  , comicCollectionHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.ComicCollectionHeader.Title as Title
import qualified StarTrek.ComicCollectionHeader.Uid as Uid

data ComicCollectionHeader = ComicCollectionHeader
  { uid :: Uid.Uid -- ^ Comic collection unique ID
  , title :: Title.Title -- ^ Comic collection title
  }
  deriving (Eq, Show)

comicCollectionHeaderSchema :: FC.Fleece schema => schema ComicCollectionHeader
comicCollectionHeaderSchema =
  FC.object $
    FC.constructor ComicCollectionHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema