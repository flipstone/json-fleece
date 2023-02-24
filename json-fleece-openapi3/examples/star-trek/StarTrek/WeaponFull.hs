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
      #+ FC.optionalField FC.OmitKey_DelegateNull "alternateReality" alternateReality FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "plasmaTechnology" plasmaTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "handHeldWeapon" handHeldWeapon FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "photonicTechnology" photonicTechnology FC.boolean
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "mirror" mirror FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "phaserTechnology" phaserTechnology FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "laserTechnology" laserTechnology FC.boolean