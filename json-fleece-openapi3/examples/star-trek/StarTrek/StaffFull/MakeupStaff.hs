{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.MakeupStaff
  ( MakeupStaff(..)
  , makeupStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MakeupStaff = MakeupStaff Bool
  deriving (Show, Eq)

makeupStaffSchema :: FC.Fleece schema => schema MakeupStaff
makeupStaffSchema =
  FC.coerceSchema FC.boolean