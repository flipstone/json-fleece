{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponHeader
  ( WeaponHeader(..)
  , weaponHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.WeaponHeader.Name as Name
import qualified StarTrek.Types.WeaponHeader.Uid as Uid

data WeaponHeader = WeaponHeader
  { uid :: Uid.Uid -- ^ Weapon unique ID
  , name :: Name.Name -- ^ Weapon name
  }
  deriving (Eq, Show)

weaponHeaderSchema :: FC.Fleece schema => schema WeaponHeader
weaponHeaderSchema =
  FC.object $
    FC.constructor WeaponHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema