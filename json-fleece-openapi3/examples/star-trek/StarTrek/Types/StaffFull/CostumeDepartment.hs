{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.CostumeDepartment
  ( CostumeDepartment(..)
  , costumeDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CostumeDepartment = CostumeDepartment Bool
  deriving (Show, Eq)

costumeDepartmentSchema :: FC.Fleece t => FC.Schema t CostumeDepartment
costumeDepartmentSchema =
  FC.coerceSchema FC.boolean