{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponBase.PhaserTechnology
  ( PhaserTechnology(..)
  , phaserTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PhaserTechnology = PhaserTechnology Bool
  deriving (Show, Eq)

phaserTechnologySchema :: FC.Fleece schema => schema PhaserTechnology
phaserTechnologySchema =
  FC.coerceSchema FC.boolean