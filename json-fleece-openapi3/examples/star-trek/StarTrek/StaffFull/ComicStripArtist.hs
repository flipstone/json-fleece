{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.ComicStripArtist
  ( ComicStripArtist(..)
  , comicStripArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicStripArtist = ComicStripArtist Bool
  deriving (Show, Eq)

comicStripArtistSchema :: FC.Fleece schema => schema ComicStripArtist
comicStripArtistSchema =
  FC.coerceSchema FC.boolean