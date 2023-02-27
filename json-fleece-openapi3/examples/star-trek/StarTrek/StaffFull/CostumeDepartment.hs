{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.CostumeDepartment
  ( CostumeDepartment(..)
  , costumeDepartmentSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CostumeDepartment = CostumeDepartment Bool
  deriving (Show, Eq)

costumeDepartmentSchema :: FC.Fleece schema => schema CostumeDepartment
costumeDepartmentSchema =
  FC.coerceSchema FC.boolean