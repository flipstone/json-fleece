{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ComicStripArtist
  ( ComicStripArtist(..)
  , comicStripArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ComicStripArtist = ComicStripArtist Bool
  deriving (Show, Eq)

comicStripArtistSchema :: FC.Fleece t => FC.Schema t ComicStripArtist
comicStripArtistSchema =
  FC.coerceSchema FC.boolean