{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.CastingDepartment
  ( CastingDepartment(..)
  , castingDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CastingDepartment = CastingDepartment Bool
  deriving (Show, Eq)

castingDepartmentSchema :: FC.Fleece schema => schema CastingDepartment
castingDepartmentSchema =
  FC.coerceSchema FC.boolean