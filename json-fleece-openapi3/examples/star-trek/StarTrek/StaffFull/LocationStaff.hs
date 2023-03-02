{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.LocationStaff
  ( LocationStaff(..)
  , locationStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LocationStaff = LocationStaff Bool
  deriving (Show, Eq)

locationStaffSchema :: FC.Fleece schema => schema LocationStaff
locationStaffSchema =
  FC.coerceSchema FC.boolean