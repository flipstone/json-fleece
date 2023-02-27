{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponBase
  ( WeaponBase(..)
  , weaponBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.WeaponBase.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.WeaponBase.HandHeldWeapon (HandHeldWeapon, handHeldWeaponSchema)
import StarTrek.WeaponBase.LaserTechnology (LaserTechnology, laserTechnologySchema)
import StarTrek.WeaponBase.Mirror (Mirror, mirrorSchema)
import StarTrek.WeaponBase.Name (Name, nameSchema)
import StarTrek.WeaponBase.PhaserTechnology (PhaserTechnology, phaserTechnologySchema)
import StarTrek.WeaponBase.PhotonicTechnology (PhotonicTechnology, photonicTechnologySchema)
import StarTrek.WeaponBase.PlasmaTechnology (PlasmaTechnology, plasmaTechnologySchema)
import StarTrek.WeaponBase.Uid (Uid, uidSchema)

data WeaponBase = WeaponBase
  { alternateReality :: Maybe AlternateReality -- ^ Whether this weapon is from alternate reality
  , name :: Name -- ^ Weapon name
  , plasmaTechnology :: Maybe PlasmaTechnology -- ^ Whether it's a plasma technology
  , handHeldWeapon :: Maybe HandHeldWeapon -- ^ Whether it's hand-help weapon
  , photonicTechnology :: Maybe PhotonicTechnology -- ^ Whether it's a photonic technology
  , uid :: Uid -- ^ Weapon unique ID
  , mirror :: Maybe Mirror -- ^ Whether this weapon is from mirror universe
  , phaserTechnology :: Maybe PhaserTechnology -- ^ Whether it's a phaser technology
  , laserTechnology :: Maybe LaserTechnology -- ^ Whether it's a laser technology
  }
  deriving (Eq, Show)

weaponBaseSchema :: FC.Fleece schema => schema WeaponBase
weaponBaseSchema =
  FC.object $
    FC.constructor WeaponBase
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "plasmaTechnology" plasmaTechnology plasmaTechnologySchema
      #+ FC.optional "handHeldWeapon" handHeldWeapon handHeldWeaponSchema
      #+ FC.optional "photonicTechnology" photonicTechnology photonicTechnologySchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "phaserTechnology" phaserTechnology phaserTechnologySchema
      #+ FC.optional "laserTechnology" laserTechnology laserTechnologySchema