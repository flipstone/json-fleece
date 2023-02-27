{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFull.HologramStatus
  ( HologramStatus(..)
  , hologramStatusSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype HologramStatus = HologramStatus Text
  deriving (Show, Eq)

hologramStatusSchema :: FC.Fleece schema => schema HologramStatus
hologramStatusSchema =
  FC.coerceSchema FC.text