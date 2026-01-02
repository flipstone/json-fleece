{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase.HumanoidSpecies
  ( HumanoidSpecies(..)
  , humanoidSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HumanoidSpecies = HumanoidSpecies Bool
  deriving (Show, Eq)

humanoidSpeciesSchema :: FC.Fleece t => FC.Schema t HumanoidSpecies
humanoidSpeciesSchema =
  FC.coerceSchema FC.boolean