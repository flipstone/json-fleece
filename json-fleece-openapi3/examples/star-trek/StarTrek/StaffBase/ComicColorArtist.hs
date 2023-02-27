{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ComicColorArtist
  ( ComicColorArtist(..)
  , comicColorArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicColorArtist = ComicColorArtist Bool
  deriving (Show, Eq)

comicColorArtistSchema :: FC.Fleece schema => schema ComicColorArtist
comicColorArtistSchema =
  FC.coerceSchema FC.boolean