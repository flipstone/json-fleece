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
  { uid :: Uid.Uid -- ^ Comic strip unique ID
  , title :: Title.Title -- ^ Comic strip title
  }
  deriving (Eq, Show)

comicStripHeaderSchema :: FC.Fleece schema => schema ComicStripHeader
comicStripHeaderSchema =
  FC.object $
    FC.constructor ComicStripHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema