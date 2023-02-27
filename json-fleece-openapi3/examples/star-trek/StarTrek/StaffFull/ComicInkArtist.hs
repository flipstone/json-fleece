{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.ComicInkArtist
  ( ComicInkArtist(..)
  , comicInkArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicInkArtist = ComicInkArtist Bool
  deriving (Show, Eq)

comicInkArtistSchema :: FC.Fleece schema => schema ComicInkArtist
comicInkArtistSchema =
  FC.coerceSchema FC.boolean