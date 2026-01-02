{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.GameArtist
  ( GameArtist(..)
  , gameArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GameArtist = GameArtist Bool
  deriving (Show, Eq)

gameArtistSchema :: FC.Fleece t => FC.Schema t GameArtist
gameArtistSchema =
  FC.coerceSchema FC.boolean