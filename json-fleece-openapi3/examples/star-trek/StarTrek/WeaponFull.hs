{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponFull
  ( WeaponFull(..)
  , weaponFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.WeaponFull.AlternateReality (AlternateReality, alternateRealitySchema)
import StarTrek.WeaponFull.HandHeldWeapon (HandHeldWeapon, handHeldWeaponSchema)
import StarTrek.WeaponFull.LaserTechnology (LaserTechnology, laserTechnologySchema)
import StarTrek.WeaponFull.Mirror (Mirror, mirrorSchema)
import StarTrek.WeaponFull.Name (Name, nameSchema)
import StarTrek.WeaponFull.PhaserTechnology (PhaserTechnology, phaserTechnologySchema)
import StarTrek.WeaponFull.PhotonicTechnology (PhotonicTechnology, photonicTechnologySchema)
import StarTrek.WeaponFull.PlasmaTechnology (PlasmaTechnology, plasmaTechnologySchema)
import StarTrek.WeaponFull.Uid (Uid, uidSchema)

data WeaponFull = WeaponFull
  { alternateReality :: Maybe AlternateReality -- ^ Whether this weapon is from alternate reality
  , name :: Name -- ^ Weapon name
  , plasmaTechnology :: Maybe PlasmaTechnology -- ^ Whether it's a plasma technology
  , handHeldWeapon :: Maybe HandHeldWeapon -- ^ Whether it's a hand-help weapon
  , photonicTechnology :: Maybe PhotonicTechnology -- ^ Whether it's a photonic technology
  , uid :: Uid -- ^ Weapon unique ID
  , mirror :: Maybe Mirror -- ^ Whether this weapon is from mirror universe
  , phaserTechnology :: Maybe PhaserTechnology -- ^ Whether it's a phaser technology
  , laserTechnology :: Maybe LaserTechnology -- ^ Whether it's a laser technology
  }
  deriving (Eq, Show)

weaponFullSchema :: FC.Fleece schema => schema WeaponFull
weaponFullSchema =
  FC.object $
    FC.constructor WeaponFull
      #+ FC.optional "alternateReality" alternateReality alternateRealitySchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "plasmaTechnology" plasmaTechnology plasmaTechnologySchema
      #+ FC.optional "handHeldWeapon" handHeldWeapon handHeldWeaponSchema
      #+ FC.optional "photonicTechnology" photonicTechnology photonicTechnologySchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "mirror" mirror mirrorSchema
      #+ FC.optional "phaserTechnology" phaserTechnology phaserTechnologySchema
      #+ FC.optional "laserTechnology" laserTechnology laserTechnologySchema