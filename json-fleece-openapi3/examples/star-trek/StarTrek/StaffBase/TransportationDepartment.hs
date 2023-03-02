{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.TransportationDepartment
  ( TransportationDepartment(..)
  , transportationDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransportationDepartment = TransportationDepartment Bool
  deriving (Show, Eq)

transportationDepartmentSchema :: FC.Fleece schema => schema TransportationDepartment
transportationDepartmentSchema =
  FC.coerceSchema FC.boolean