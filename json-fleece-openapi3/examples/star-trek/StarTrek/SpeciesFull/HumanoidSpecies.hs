{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFull.HumanoidSpecies
  ( HumanoidSpecies(..)
  , humanoidSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HumanoidSpecies = HumanoidSpecies Bool
  deriving (Show, Eq)

humanoidSpeciesSchema :: FC.Fleece schema => schema HumanoidSpecies
humanoidSpeciesSchema =
  FC.coerceSchema FC.boolean