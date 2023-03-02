{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.SpecialFeaturesStaff
  ( SpecialFeaturesStaff(..)
  , specialFeaturesStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SpecialFeaturesStaff = SpecialFeaturesStaff Bool
  deriving (Show, Eq)

specialFeaturesStaffSchema :: FC.Fleece schema => schema SpecialFeaturesStaff
specialFeaturesStaffSchema =
  FC.coerceSchema FC.boolean