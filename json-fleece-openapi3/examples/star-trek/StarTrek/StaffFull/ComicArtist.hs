{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.ComicArtist
  ( ComicArtist(..)
  , comicArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicArtist = ComicArtist Bool
  deriving (Show, Eq)

comicArtistSchema :: FC.Fleece schema => schema ComicArtist
comicArtistSchema =
  FC.coerceSchema FC.boolean