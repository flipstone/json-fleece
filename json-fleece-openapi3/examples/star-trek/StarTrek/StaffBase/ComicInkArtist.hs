{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ComicInkArtist
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