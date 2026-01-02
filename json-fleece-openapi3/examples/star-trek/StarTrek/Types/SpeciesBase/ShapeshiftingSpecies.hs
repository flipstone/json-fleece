{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase.ShapeshiftingSpecies
  ( ShapeshiftingSpecies(..)
  , shapeshiftingSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ShapeshiftingSpecies = ShapeshiftingSpecies Bool
  deriving (Show, Eq)

shapeshiftingSpeciesSchema :: FC.Fleece t => FC.Schema t ShapeshiftingSpecies
shapeshiftingSpeciesSchema =
  FC.coerceSchema FC.boolean