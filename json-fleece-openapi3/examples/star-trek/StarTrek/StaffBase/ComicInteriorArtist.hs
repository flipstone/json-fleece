{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ComicInteriorArtist
  ( ComicInteriorArtist(..)
  , comicInteriorArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicInteriorArtist = ComicInteriorArtist Bool
  deriving (Show, Eq)

comicInteriorArtistSchema :: FC.Fleece schema => schema ComicInteriorArtist
comicInteriorArtistSchema =
  FC.coerceSchema FC.boolean