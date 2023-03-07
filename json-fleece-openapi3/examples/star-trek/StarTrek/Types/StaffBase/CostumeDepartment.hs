{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.CostumeDepartment
  ( CostumeDepartment(..)
  , costumeDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CostumeDepartment = CostumeDepartment Bool
  deriving (Show, Eq)

costumeDepartmentSchema :: FC.Fleece schema => schema CostumeDepartment
costumeDepartmentSchema =
  FC.coerceSchema FC.boolean