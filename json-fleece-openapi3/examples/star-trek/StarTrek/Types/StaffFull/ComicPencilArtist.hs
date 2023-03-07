{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ComicPencilArtist
  ( ComicPencilArtist(..)
  , comicPencilArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicPencilArtist = ComicPencilArtist Bool
  deriving (Show, Eq)

comicPencilArtistSchema :: FC.Fleece schema => schema ComicPencilArtist
comicPencilArtistSchema =
  FC.coerceSchema FC.boolean