{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionHeader
  ( ComicCollectionHeader(..)
  , comicCollectionHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data ComicCollectionHeader = ComicCollectionHeader
  { uid :: Text -- ^ Comic collection unique ID
  , title :: Text -- ^ Comic collection title
  }
  deriving (Eq, Show)

comicCollectionHeaderSchema :: FC.Fleece schema => schema ComicCollectionHeader
comicCollectionHeaderSchema =
  FC.object $
    FC.constructor ComicCollectionHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text