{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesHeader
  ( ComicSeriesHeader(..)
  , comicSeriesHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data ComicSeriesHeader = ComicSeriesHeader
  { uid :: Text -- ^ Comic series unique ID
  , title :: Text -- ^ Comic series title
  }
  deriving (Eq, Show)

comicSeriesHeaderSchema :: FC.Fleece schema => schema ComicSeriesHeader
comicSeriesHeaderSchema =
  FC.object $
    FC.constructor ComicSeriesHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text