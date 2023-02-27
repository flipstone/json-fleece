{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBase.HumanoidSpecies
  ( HumanoidSpecies(..)
  , humanoidSpeciesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HumanoidSpecies = HumanoidSpecies Bool
  deriving (Show, Eq)

humanoidSpeciesSchema :: FC.Fleece schema => schema HumanoidSpecies
humanoidSpeciesSchema =
  FC.coerceSchema FC.boolean