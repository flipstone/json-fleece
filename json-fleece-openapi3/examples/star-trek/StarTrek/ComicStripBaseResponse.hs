{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripBaseResponse
  ( ComicStripBaseResponse(..)
  , comicStripBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicStripBase (ComicStripBase, comicStripBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data ComicStripBaseResponse = ComicStripBaseResponse
  { comicStrips :: Maybe [ComicStripBase] -- ^ List of comic strips matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

comicStripBaseResponseSchema :: FC.Fleece schema => schema ComicStripBaseResponse
comicStripBaseResponseSchema =
  FC.object $
    FC.constructor ComicStripBaseResponse
      #+ FC.optional "comicStrips" comicStrips (FC.list comicStripBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema