{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponFull
  ( WeaponFull(..)
  , weaponFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data WeaponFull = WeaponFull
  { alternateReality :: Maybe Bool -- ^ Whether this weapon is from alternate reality
  , name :: Text -- ^ Weapon name
  , plasmaTechnology :: Maybe Bool -- ^ Whether it's a plasma technology
  , handHeldWeapon :: Maybe Bool -- ^ Whether it's a hand-help weapon
  , photonicTechnology :: Maybe Bool -- ^ Whether it's a photonic technology
  , uid :: Text -- ^ Weapon unique ID
  , mirror :: Maybe Bool -- ^ Whether this weapon is from mirror universe
  , phaserTechnology :: Maybe Bool -- ^ Whether it's a phaser technology
  , laserTechnology :: Maybe Bool -- ^ Whether it's a laser technology
  }
  deriving (Eq, Show)

weaponFullSchema :: FC.Fleece schema => schema WeaponFull
weaponFullSchema =
  FC.object $
    FC.constructor WeaponFull
      #+ FC.optional "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optional "plasmaTechnology" plasmaTechnology FC.boolean
      #+ FC.optional "handHeldWeapon" handHeldWeapon FC.boolean
      #+ FC.optional "photonicTechnology" photonicTechnology FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "mirror" mirror FC.boolean
      #+ FC.optional "phaserTechnology" phaserTechnology FC.boolean
      #+ FC.optional "laserTechnology" laserTechnology FC.boolean