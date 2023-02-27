{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.CastingDepartment
  ( CastingDepartment(..)
  , castingDepartmentSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CastingDepartment = CastingDepartment Bool
  deriving (Show, Eq)

castingDepartmentSchema :: FC.Fleece schema => schema CastingDepartment
castingDepartmentSchema =
  FC.coerceSchema FC.boolean