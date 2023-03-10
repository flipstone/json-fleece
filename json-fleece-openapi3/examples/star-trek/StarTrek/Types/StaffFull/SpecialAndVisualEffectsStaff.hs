{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.SpecialAndVisualEffectsStaff
  ( SpecialAndVisualEffectsStaff(..)
  , specialAndVisualEffectsStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SpecialAndVisualEffectsStaff = SpecialAndVisualEffectsStaff Bool
  deriving (Show, Eq)

specialAndVisualEffectsStaffSchema :: FC.Fleece schema => schema SpecialAndVisualEffectsStaff
specialAndVisualEffectsStaffSchema =
  FC.coerceSchema FC.boolean