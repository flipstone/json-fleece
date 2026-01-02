{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponBase.PhaserTechnology
  ( PhaserTechnology(..)
  , phaserTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PhaserTechnology = PhaserTechnology Bool
  deriving (Show, Eq)

phaserTechnologySchema :: FC.Fleece t => FC.Schema t PhaserTechnology
phaserTechnologySchema =
  FC.coerceSchema FC.boolean