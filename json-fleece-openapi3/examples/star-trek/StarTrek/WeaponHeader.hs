{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponHeader
  ( WeaponHeader(..)
  , weaponHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.WeaponHeader.Name as Name
import qualified StarTrek.WeaponHeader.Uid as Uid

data WeaponHeader = WeaponHeader
  { name :: Name.Name -- ^ Weapon name
  , uid :: Uid.Uid -- ^ Weapon unique ID
  }
  deriving (Eq, Show)

weaponHeaderSchema :: FC.Fleece schema => schema WeaponHeader
weaponHeaderSchema =
  FC.object $
    FC.constructor WeaponHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema