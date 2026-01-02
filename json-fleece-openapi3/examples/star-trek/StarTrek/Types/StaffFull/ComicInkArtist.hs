{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ComicInkArtist
  ( ComicInkArtist(..)
  , comicInkArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicInkArtist = ComicInkArtist Bool
  deriving (Show, Eq)

comicInkArtistSchema :: FC.Fleece t => FC.Schema t ComicInkArtist
comicInkArtistSchema =
  FC.coerceSchema FC.boolean