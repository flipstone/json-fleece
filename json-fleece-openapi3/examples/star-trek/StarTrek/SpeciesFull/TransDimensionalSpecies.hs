{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFull.TransDimensionalSpecies
  ( TransDimensionalSpecies(..)
  , transDimensionalSpeciesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransDimensionalSpecies = TransDimensionalSpecies Bool
  deriving (Show, Eq)

transDimensionalSpeciesSchema :: FC.Fleece schema => schema TransDimensionalSpecies
transDimensionalSpeciesSchema =
  FC.coerceSchema FC.boolean