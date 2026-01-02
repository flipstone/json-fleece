{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ComicPencilArtist
  ( ComicPencilArtist(..)
  , comicPencilArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicPencilArtist = ComicPencilArtist Bool
  deriving (Show, Eq)

comicPencilArtistSchema :: FC.Fleece t => FC.Schema t ComicPencilArtist
comicPencilArtistSchema =
  FC.coerceSchema FC.boolean