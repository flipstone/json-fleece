{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicCollectionHeader
  ( ComicCollectionHeader(..)
  , comicCollectionHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.ComicCollectionHeader.Title as Title
import qualified StarTrek.Types.ComicCollectionHeader.Uid as Uid

data ComicCollectionHeader = ComicCollectionHeader
  { title :: Title.Title -- ^ Comic collection title
  , uid :: Uid.Uid -- ^ Comic collection unique ID
  }
  deriving (Eq, Show)

comicCollectionHeaderSchema :: FC.Fleece t => FC.Schema t ComicCollectionHeader
comicCollectionHeaderSchema =
  FC.object $
    FC.constructor ComicCollectionHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema