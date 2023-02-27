{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripHeader
  ( ComicStripHeader(..)
  , comicStripHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.ComicStripHeader.Title (Title, titleSchema)
import StarTrek.ComicStripHeader.Uid (Uid, uidSchema)

data ComicStripHeader = ComicStripHeader
  { uid :: Uid -- ^ Comic strip unique ID
  , title :: Title -- ^ Comic strip title
  }
  deriving (Eq, Show)

comicStripHeaderSchema :: FC.Fleece schema => schema ComicStripHeader
comicStripHeaderSchema =
  FC.object $
    FC.constructor ComicStripHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema