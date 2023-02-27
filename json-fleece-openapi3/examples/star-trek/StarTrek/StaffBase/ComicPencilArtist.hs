{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ComicPencilArtist
  ( ComicPencilArtist(..)
  , comicPencilArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicPencilArtist = ComicPencilArtist Bool
  deriving (Show, Eq)

comicPencilArtistSchema :: FC.Fleece schema => schema ComicPencilArtist
comicPencilArtistSchema =
  FC.coerceSchema FC.boolean