{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.IlmProductionStaff
  ( IlmProductionStaff(..)
  , ilmProductionStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype IlmProductionStaff = IlmProductionStaff Bool
  deriving (Show, Eq)

ilmProductionStaffSchema :: FC.Fleece t => FC.Schema t IlmProductionStaff
ilmProductionStaffSchema =
  FC.coerceSchema FC.boolean