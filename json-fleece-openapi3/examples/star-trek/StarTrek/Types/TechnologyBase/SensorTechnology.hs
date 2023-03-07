{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.SensorTechnology
  ( SensorTechnology(..)
  , sensorTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SensorTechnology = SensorTechnology Bool
  deriving (Show, Eq)

sensorTechnologySchema :: FC.Fleece schema => schema SensorTechnology
sensorTechnologySchema =
  FC.coerceSchema FC.boolean