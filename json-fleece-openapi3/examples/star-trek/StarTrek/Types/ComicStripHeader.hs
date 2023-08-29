{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripHeader
  ( ComicStripHeader(..)
  , comicStripHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.ComicStripHeader.Title as Title
import qualified StarTrek.Types.ComicStripHeader.Uid as Uid

data ComicStripHeader = ComicStripHeader
  { title :: Title.Title -- ^ Comic strip title
  , uid :: Uid.Uid -- ^ Comic strip unique ID
  }
  deriving (Eq, Show)

comicStripHeaderSchema :: FC.Fleece schema => schema ComicStripHeader
comicStripHeaderSchema =
  FC.object $
    FC.constructor ComicStripHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema