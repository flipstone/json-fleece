{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.NovelArtist
  ( NovelArtist(..)
  , novelArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype NovelArtist = NovelArtist Bool
  deriving (Show, Eq)

novelArtistSchema :: FC.Fleece schema => schema NovelArtist
novelArtistSchema =
  FC.coerceSchema FC.boolean