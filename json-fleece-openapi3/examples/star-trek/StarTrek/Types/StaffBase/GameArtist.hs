{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.GameArtist
  ( GameArtist(..)
  , gameArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GameArtist = GameArtist Bool
  deriving (Show, Eq)

gameArtistSchema :: FC.Fleece schema => schema GameArtist
gameArtistSchema =
  FC.coerceSchema FC.boolean