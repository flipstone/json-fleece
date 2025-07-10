{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponFull
  ( WeaponFull(..)
  , weaponFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.WeaponFull.AlternateReality as AlternateReality
import qualified StarTrek.Types.WeaponFull.HandHeldWeapon as HandHeldWeapon
import qualified StarTrek.Types.WeaponFull.LaserTechnology as LaserTechnology
import qualified StarTrek.Types.WeaponFull.Mirror as Mirror
import qualified StarTrek.Types.WeaponFull.Name as Name
import qualified StarTrek.Types.WeaponFull.PhaserTechnology as PhaserTechnology
import qualified StarTrek.Types.WeaponFull.PhotonicTechnology as PhotonicTechnology
import qualified StarTrek.Types.WeaponFull.PlasmaTechnology as PlasmaTechnology
import qualified StarTrek.Types.WeaponFull.Uid as Uid

data WeaponFull = WeaponFull
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this weapon is from alternate reality
  , handHeldWeapon :: Maybe HandHeldWeapon.HandHeldWeapon -- ^ Whether it's a hand-help weapon
  , laserTechnology :: Maybe LaserTechnology.LaserTechnology -- ^ Whether it's a laser technology
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this weapon is from mirror universe
  , name :: Name.Name -- ^ Weapon name
  , phaserTechnology :: Maybe PhaserTechnology.PhaserTechnology -- ^ Whether it's a phaser technology
  , photonicTechnology :: Maybe PhotonicTechnology.PhotonicTechnology -- ^ Whether it's a photonic technology
  , plasmaTechnology :: Maybe PlasmaTechnology.PlasmaTechnology -- ^ Whether it's a plasma technology
  , uid :: Uid.Uid -- ^ Weapon unique ID
  }
  deriving (Eq, Show)

weaponFullSchema :: FC.Fleece schema => schema WeaponFull
weaponFullSchema =
  FC.object $
    FC.constructor WeaponFull
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "handHeldWeapon" handHeldWeapon HandHeldWeapon.handHeldWeaponSchema
      #+ FC.optional "laserTechnology" laserTechnology LaserTechnology.laserTechnologySchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "phaserTechnology" phaserTechnology PhaserTechnology.phaserTechnologySchema
      #+ FC.optional "photonicTechnology" photonicTechnology PhotonicTechnology.photonicTechnologySchema
      #+ FC.optional "plasmaTechnology" plasmaTechnology PlasmaTechnology.plasmaTechnologySchema
      #+ FC.required "uid" uid Uid.uidSchema