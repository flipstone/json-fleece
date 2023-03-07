{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.IlmProductionStaff
  ( IlmProductionStaff(..)
  , ilmProductionStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype IlmProductionStaff = IlmProductionStaff Bool
  deriving (Show, Eq)

ilmProductionStaffSchema :: FC.Fleece schema => schema IlmProductionStaff
ilmProductionStaffSchema =
  FC.coerceSchema FC.boolean