{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesHeader
  ( ComicSeriesHeader(..)
  , comicSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.ComicSeriesHeader.Title (Title, titleSchema)
import StarTrek.ComicSeriesHeader.Uid (Uid, uidSchema)

data ComicSeriesHeader = ComicSeriesHeader
  { uid :: Uid -- ^ Comic series unique ID
  , title :: Title -- ^ Comic series title
  }
  deriving (Eq, Show)

comicSeriesHeaderSchema :: FC.Fleece schema => schema ComicSeriesHeader
comicSeriesHeaderSchema =
  FC.object $
    FC.constructor ComicSeriesHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema