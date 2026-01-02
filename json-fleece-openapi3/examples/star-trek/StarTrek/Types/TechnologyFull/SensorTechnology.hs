{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.SensorTechnology
  ( SensorTechnology(..)
  , sensorTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SensorTechnology = SensorTechnology Bool
  deriving (Show, Eq)

sensorTechnologySchema :: FC.Fleece t => FC.Schema t SensorTechnology
sensorTechnologySchema =
  FC.coerceSchema FC.boolean