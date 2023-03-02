{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.ExhibitAndAttractionStaff
  ( ExhibitAndAttractionStaff(..)
  , exhibitAndAttractionStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ExhibitAndAttractionStaff = ExhibitAndAttractionStaff Bool
  deriving (Show, Eq)

exhibitAndAttractionStaffSchema :: FC.Fleece schema => schema ExhibitAndAttractionStaff
exhibitAndAttractionStaffSchema =
  FC.coerceSchema FC.boolean