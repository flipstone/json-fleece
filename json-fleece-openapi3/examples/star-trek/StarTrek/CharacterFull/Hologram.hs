{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.Hologram
  ( Hologram(..)
  , hologramSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Hologram = Hologram Bool
  deriving (Show, Eq)

hologramSchema :: FC.Fleece schema => schema Hologram
hologramSchema =
  FC.coerceSchema FC.boolean