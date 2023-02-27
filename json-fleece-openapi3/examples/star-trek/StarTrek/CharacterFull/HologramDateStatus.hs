{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.HologramDateStatus
  ( HologramDateStatus(..)
  , hologramDateStatusSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HologramDateStatus = HologramDateStatus Text
  deriving (Show, Eq)

hologramDateStatusSchema :: FC.Fleece schema => schema HologramDateStatus
hologramDateStatusSchema =
  FC.coerceSchema FC.text