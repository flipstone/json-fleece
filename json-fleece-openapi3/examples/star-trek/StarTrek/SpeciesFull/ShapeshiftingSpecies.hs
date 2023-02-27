{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFull.ShapeshiftingSpecies
  ( ShapeshiftingSpecies(..)
  , shapeshiftingSpeciesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ShapeshiftingSpecies = ShapeshiftingSpecies Bool
  deriving (Show, Eq)

shapeshiftingSpeciesSchema :: FC.Fleece schema => schema ShapeshiftingSpecies
shapeshiftingSpeciesSchema =
  FC.coerceSchema FC.boolean