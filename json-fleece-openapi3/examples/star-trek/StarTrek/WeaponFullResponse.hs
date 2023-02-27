{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponFullResponse
  ( WeaponFullResponse(..)
  , weaponFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.WeaponFull (WeaponFull, weaponFullSchema)

data WeaponFullResponse = WeaponFullResponse
  { weapon :: Maybe WeaponFull -- ^ Full weapon, returned when queried using UID
  }
  deriving (Eq, Show)

weaponFullResponseSchema :: FC.Fleece schema => schema WeaponFullResponse
weaponFullResponseSchema =
  FC.object $
    FC.constructor WeaponFullResponse
      #+ FC.optional "weapon" weapon weaponFullSchema