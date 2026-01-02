{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponBase.HandHeldWeapon
  ( HandHeldWeapon(..)
  , handHeldWeaponSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HandHeldWeapon = HandHeldWeapon Bool
  deriving (Show, Eq)

handHeldWeaponSchema :: FC.Fleece t => FC.Schema t HandHeldWeapon
handHeldWeaponSchema =
  FC.coerceSchema FC.boolean