{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.SensorTechnology
  ( SensorTechnology(..)
  , sensorTechnologySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SensorTechnology = SensorTechnology Bool
  deriving (Show, Eq)

sensorTechnologySchema :: FC.Fleece schema => schema SensorTechnology
sensorTechnologySchema =
  FC.coerceSchema FC.boolean