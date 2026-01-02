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
  { title :: Title.Title -- ^ Comic series title
  , uid :: Uid.Uid -- ^ Comic series unique ID
  }
  deriving (Eq, Show)

comicSeriesHeaderSchema :: FC.Fleece t => FC.Schema t ComicSeriesHeader
comicSeriesHeaderSchema =
  FC.object $
    FC.constructor ComicSeriesHeader
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema