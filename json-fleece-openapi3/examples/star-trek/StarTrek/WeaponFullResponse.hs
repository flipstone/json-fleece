{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponFullResponse
  ( WeaponFullResponse(..)
  , weaponFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.WeaponFull as WeaponFull

data WeaponFullResponse = WeaponFullResponse
  { weapon :: Maybe WeaponFull.WeaponFull -- ^ Full weapon, returned when queried using UID
  }
  deriving (Eq, Show)

weaponFullResponseSchema :: FC.Fleece schema => schema WeaponFullResponse
weaponFullResponseSchema =
  FC.object $
    FC.constructor WeaponFullResponse
      #+ FC.optional "weapon" weapon WeaponFull.weaponFullSchema