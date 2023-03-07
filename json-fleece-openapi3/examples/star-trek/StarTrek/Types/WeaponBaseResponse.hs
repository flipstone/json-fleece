{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponBaseResponse
  ( WeaponBaseResponse(..)
  , weaponBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.WeaponBase as WeaponBase

data WeaponBaseResponse = WeaponBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , weapons :: Maybe [WeaponBase.WeaponBase] -- ^ Base weapon, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

weaponBaseResponseSchema :: FC.Fleece schema => schema WeaponBaseResponse
weaponBaseResponseSchema =
  FC.object $
    FC.constructor WeaponBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "weapons" weapons (FC.list WeaponBase.weaponBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema