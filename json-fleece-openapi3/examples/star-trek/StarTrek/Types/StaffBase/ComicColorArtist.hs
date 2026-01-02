{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ComicColorArtist
  ( ComicColorArtist(..)
  , comicColorArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicColorArtist = ComicColorArtist Bool
  deriving (Show, Eq)

comicColorArtistSchema :: FC.Fleece t => FC.Schema t ComicColorArtist
comicColorArtistSchema =
  FC.coerceSchema FC.boolean