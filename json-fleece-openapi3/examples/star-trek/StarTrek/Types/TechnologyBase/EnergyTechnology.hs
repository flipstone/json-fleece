{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.EnergyTechnology
  ( EnergyTechnology(..)
  , energyTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EnergyTechnology = EnergyTechnology Bool
  deriving (Show, Eq)

energyTechnologySchema :: FC.Fleece t => FC.Schema t EnergyTechnology
energyTechnologySchema =
  FC.coerceSchema FC.boolean