{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponHeader
  ( WeaponHeader(..)
  , weaponHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.WeaponHeader.Name (Name, nameSchema)
import StarTrek.WeaponHeader.Uid (Uid, uidSchema)

data WeaponHeader = WeaponHeader
  { name :: Name -- ^ Weapon name
  , uid :: Uid -- ^ Weapon unique ID
  }
  deriving (Eq, Show)

weaponHeaderSchema :: FC.Fleece schema => schema WeaponHeader
weaponHeaderSchema =
  FC.object $
    FC.constructor WeaponHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema