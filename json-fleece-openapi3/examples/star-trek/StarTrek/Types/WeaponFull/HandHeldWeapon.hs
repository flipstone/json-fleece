{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponFull.HandHeldWeapon
  ( HandHeldWeapon(..)
  , handHeldWeaponSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HandHeldWeapon = HandHeldWeapon Bool
  deriving (Show, Eq)

handHeldWeaponSchema :: FC.Fleece schema => schema HandHeldWeapon
handHeldWeaponSchema =
  FC.coerceSchema FC.boolean