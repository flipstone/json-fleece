{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripFullResponse
  ( ComicStripFullResponse(..)
  , comicStripFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicStripFull (ComicStripFull, comicStripFullSchema)

data ComicStripFullResponse = ComicStripFullResponse
  { comicStrip :: Maybe ComicStripFull -- ^ Full comic strip, returned when queried using UID
  }
  deriving (Eq, Show)

comicStripFullResponseSchema :: FC.Fleece schema => schema ComicStripFullResponse
comicStripFullResponseSchema =
  FC.object $
    FC.constructor ComicStripFullResponse
      #+ FC.optional "comicStrip" comicStrip comicStripFullSchema