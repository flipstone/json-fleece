{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ExhibitAndAttractionStaff
  ( ExhibitAndAttractionStaff(..)
  , exhibitAndAttractionStaffSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ExhibitAndAttractionStaff = ExhibitAndAttractionStaff Bool
  deriving (Show, Eq)

exhibitAndAttractionStaffSchema :: FC.Fleece schema => schema ExhibitAndAttractionStaff
exhibitAndAttractionStaffSchema =
  FC.coerceSchema FC.boolean