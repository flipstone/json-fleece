{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripFullResponse
  ( ComicStripFullResponse(..)
  , comicStripFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicStripFull as ComicStripFull

data ComicStripFullResponse = ComicStripFullResponse
  { comicStrip :: Maybe ComicStripFull.ComicStripFull -- ^ Full comic strip, returned when queried using UID
  }
  deriving (Eq, Show)

comicStripFullResponseSchema :: FC.Fleece t => FC.Schema t ComicStripFullResponse
comicStripFullResponseSchema =
  FC.object $
    FC.constructor ComicStripFullResponse
      #+ FC.optional "comicStrip" comicStrip ComicStripFull.comicStripFullSchema