{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.Hologram
  ( Hologram(..)
  , hologramSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Hologram = Hologram Bool
  deriving (Show, Eq)

hologramSchema :: FC.Fleece t => FC.Schema t Hologram
hologramSchema =
  FC.coerceSchema FC.boolean