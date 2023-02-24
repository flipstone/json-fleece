{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripHeader
  ( ComicStripHeader(..)
  , comicStripHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data ComicStripHeader = ComicStripHeader
  { uid :: Text -- ^ Comic strip unique ID
  , title :: Text -- ^ Comic strip title
  }
  deriving (Eq, Show)

comicStripHeaderSchema :: FC.Fleece schema => schema ComicStripHeader
comicStripHeaderSchema =
  FC.object $
    FC.constructor ComicStripHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text