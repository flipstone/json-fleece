{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesHeader
  ( ComicSeriesHeader(..)
  , comicSeriesHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.ComicSeriesHeader.Title as Title
import qualified StarTrek.Types.ComicSeriesHeader.Uid as Uid

data ComicSeriesHeader = ComicSeriesHeader
  { uid :: Uid.Uid -- ^ Comic series unique ID
  , title :: Title.Title -- ^ Comic series title
  }
  deriving (Eq, Show)

comicSeriesHeaderSchema :: FC.Fleece schema => schema ComicSeriesHeader
comicSeriesHeaderSchema =
  FC.object $
    FC.constructor ComicSeriesHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema