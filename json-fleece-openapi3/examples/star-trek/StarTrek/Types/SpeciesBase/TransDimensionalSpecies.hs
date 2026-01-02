{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase.TransDimensionalSpecies
  ( TransDimensionalSpecies(..)
  , transDimensionalSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransDimensionalSpecies = TransDimensionalSpecies Bool
  deriving (Show, Eq)

transDimensionalSpeciesSchema :: FC.Fleece t => FC.Schema t TransDimensionalSpecies
transDimensionalSpeciesSchema =
  FC.coerceSchema FC.boolean