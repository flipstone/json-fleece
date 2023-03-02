{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBase.ShapeshiftingSpecies
  ( ShapeshiftingSpecies(..)
  , shapeshiftingSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ShapeshiftingSpecies = ShapeshiftingSpecies Bool
  deriving (Show, Eq)

shapeshiftingSpeciesSchema :: FC.Fleece schema => schema ShapeshiftingSpecies
shapeshiftingSpeciesSchema =
  FC.coerceSchema FC.boolean