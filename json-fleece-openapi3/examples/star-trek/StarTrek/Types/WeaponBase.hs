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
  , mirror :: Maybe Mirror.Mirror -- ^ Whether this weapon is from mirror universe
  , uid :: Uid.Uid -- ^ Weapon unique ID
  , photonicTechnology :: Maybe PhotonicTechnology.PhotonicTechnology -- ^ Whether it's a photonic technology
  , handHeldWeapon :: Maybe HandHeldWeapon.HandHeldWeapon -- ^ Whether it's hand-help weapon
  , laserTechnology :: Maybe LaserTechnology.LaserTechnology -- ^ Whether it's a laser technology
  , phaserTechnology :: Maybe PhaserTechnology.PhaserTechnology -- ^ Whether it's a phaser technology
  , plasmaTechnology :: Maybe PlasmaTechnology.PlasmaTechnology -- ^ Whether it's a plasma technology
  , name :: Name.Name -- ^ Weapon name
  }
  deriving (Eq, Show)

weaponBaseSchema :: FC.Fleece schema => schema WeaponBase
weaponBaseSchema =
  FC.object $
    FC.constructor WeaponBase
      #+ FC.optional "alternateReality" alternateReality AlternateReality.alternateRealitySchema
      #+ FC.optional "mirror" mirror Mirror.mirrorSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "photonicTechnology" photonicTechnology PhotonicTechnology.photonicTechnologySchema
      #+ FC.optional "handHeldWeapon" handHeldWeapon HandHeldWeapon.handHeldWeaponSchema
      #+ FC.optional "laserTechnology" laserTechnology LaserTechnology.laserTechnologySchema
      #+ FC.optional "phaserTechnology" phaserTechnology PhaserTechnology.phaserTechnologySchema
      #+ FC.optional "plasmaTechnology" plasmaTechnology PlasmaTechnology.plasmaTechnologySchema
      #+ FC.required "name" name Name.nameSchema