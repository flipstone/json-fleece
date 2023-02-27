{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ComicArtist
  ( ComicArtist(..)
  , comicArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicArtist = ComicArtist Bool
  deriving (Show, Eq)

comicArtistSchema :: FC.Fleece schema => schema ComicArtist
comicArtistSchema =
  FC.coerceSchema FC.boolean