{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.SpecialFeaturesStaff
  ( SpecialFeaturesStaff(..)
  , specialFeaturesStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SpecialFeaturesStaff = SpecialFeaturesStaff Bool
  deriving (Show, Eq)

specialFeaturesStaffSchema :: FC.Fleece t => FC.Schema t SpecialFeaturesStaff
specialFeaturesStaffSchema =
  FC.coerceSchema FC.boolean