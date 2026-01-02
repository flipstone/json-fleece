{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponBase
  ( WeaponBase(..)
  , weaponBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.WeaponBase.AlternateReality as AlternateReality
import qualified StarTrek.Types.WeaponBase.HandHeldWeapon as HandHeldWeapon
import qualified StarTrek.Types.WeaponBase.LaserTechnology as LaserTechnology
import qualified StarTrek.Types.WeaponBase.Mirror as Mirror
import qualified StarTrek.Types.WeaponBase.Name as Name
import qualified StarTrek.Types.WeaponBase.PhaserTechnology as PhaserTechnology
import qualified StarTrek.Types.WeaponBase.PhotonicTechnology as PhotonicTechnology
import qualified StarTrek.Types.WeaponBase.PlasmaTechnology as PlasmaTechnology
import qualified StarTrek.Types.WeaponBase.Uid as Uid

data WeaponBase = WeaponBase
  { alternateReality :: Maybe AlternateReality.AlternateReality -- ^ Whether this weapon is from alternate reality
  , handHeldWeapon :: Maybe HandHeldWeapon.HandHeldWeapon -- ^ Whether it's hand-help weapon
  , laserTechnology :: Maybe LaserTechnology.LaserTechnology -- ^ Whether it's a laser technology
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this weapon is from mirror universe
  , name :: Name.Name -- ^ Weapon name
  , phaserTechnology :: Maybe PhaserTechnology.PhaserTechnology -- ^ Whether it's a phaser technology
  , photonicTechnology :: Maybe PhotonicTechnology.PhotonicTechnology -- ^ Whether it's a photonic technology
  , plasmaTechnology :: Maybe PlasmaTechnology.PlasmaTechnology -- ^ Whether it's a plasma technology
  , uid :: Uid.Uid -- ^ Weapon unique ID
  }
  deriving (Eq, Show)

weaponBaseSchema :: FC.Fleece t => FC.Schema t WeaponBase
weaponBaseSchema =
  FC.object $
    FC.constructor WeaponBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "handHeldWeapon" handHeldWeapon HandHeldWeapon.handHeldWeaponSchema
      #+ FC.optional "laserTechnology" laserTechnology LaserTechnology.laserTechnologySchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "phaserTechnology" phaserTechnology PhaserTechnology.phaserTechnologySchema
      #+ FC.optional "photonicTechnology" photonicTechnology PhotonicTechnology.photonicTechnologySchema
      #+ FC.optional "plasmaTechnology" plasmaTechnology PlasmaTechnology.plasmaTechnologySchema
      #+ FC.required "uid" uid Uid.uidSchema