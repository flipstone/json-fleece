{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.EnergyTechnology
  ( EnergyTechnology(..)
  , energyTechnologySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EnergyTechnology = EnergyTechnology Bool
  deriving (Show, Eq)

energyTechnologySchema :: FC.Fleece schema => schema EnergyTechnology
energyTechnologySchema =
  FC.coerceSchema FC.boolean