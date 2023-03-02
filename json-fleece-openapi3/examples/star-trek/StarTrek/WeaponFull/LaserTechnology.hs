{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponFull.LaserTechnology
  ( LaserTechnology(..)
  , laserTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LaserTechnology = LaserTechnology Bool
  deriving (Show, Eq)

laserTechnologySchema :: FC.Fleece schema => schema LaserTechnology
laserTechnologySchema =
  FC.coerceSchema FC.boolean