{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ComicInteriorArtist
  ( ComicInteriorArtist(..)
  , comicInteriorArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicInteriorArtist = ComicInteriorArtist Bool
  deriving (Show, Eq)

comicInteriorArtistSchema :: FC.Fleece t => FC.Schema t ComicInteriorArtist
comicInteriorArtistSchema =
  FC.coerceSchema FC.boolean