{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponFull.LaserTechnology
  ( LaserTechnology(..)
  , laserTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LaserTechnology = LaserTechnology Bool
  deriving (Show, Eq)

laserTechnologySchema :: FC.Fleece t => FC.Schema t LaserTechnology
laserTechnologySchema =
  FC.coerceSchema FC.boolean