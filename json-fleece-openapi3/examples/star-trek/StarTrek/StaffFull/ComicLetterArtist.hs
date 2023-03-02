{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.ComicLetterArtist
  ( ComicLetterArtist(..)
  , comicLetterArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicLetterArtist = ComicLetterArtist Bool
  deriving (Show, Eq)

comicLetterArtistSchema :: FC.Fleece schema => schema ComicLetterArtist
comicLetterArtistSchema =
  FC.coerceSchema FC.boolean