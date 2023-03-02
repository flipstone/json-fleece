{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.CbsDigitalStaff
  ( CbsDigitalStaff(..)
  , cbsDigitalStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CbsDigitalStaff = CbsDigitalStaff Bool
  deriving (Show, Eq)

cbsDigitalStaffSchema :: FC.Fleece schema => schema CbsDigitalStaff
cbsDigitalStaffSchema =
  FC.coerceSchema FC.boolean