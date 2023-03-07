{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ComicLetterArtist
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