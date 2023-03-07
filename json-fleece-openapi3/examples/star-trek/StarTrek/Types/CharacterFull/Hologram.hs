{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterFull.Hologram
  ( Hologram(..)
  , hologramSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Hologram = Hologram Bool
  deriving (Show, Eq)

hologramSchema :: FC.Fleece schema => schema Hologram
hologramSchema =
  FC.coerceSchema FC.boolean