{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponHeader
  ( WeaponHeader(..)
  , weaponHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data WeaponHeader = WeaponHeader
  { name :: Text -- ^ Weapon name
  , uid :: Text -- ^ Weapon unique ID
  }
  deriving (Eq, Show)

weaponHeaderSchema :: FC.Fleece schema => schema WeaponHeader
weaponHeaderSchema =
  FC.object $
    FC.constructor WeaponHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text