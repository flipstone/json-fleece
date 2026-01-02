{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.LocationStaff
  ( LocationStaff(..)
  , locationStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype LocationStaff = LocationStaff Bool
  deriving (Show, Eq)

locationStaffSchema :: FC.Fleece t => FC.Schema t LocationStaff
locationStaffSchema =
  FC.coerceSchema FC.boolean