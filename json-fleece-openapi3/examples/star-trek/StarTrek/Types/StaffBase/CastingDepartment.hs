{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.CastingDepartment
  ( CastingDepartment(..)
  , castingDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CastingDepartment = CastingDepartment Bool
  deriving (Show, Eq)

castingDepartmentSchema :: FC.Fleece t => FC.Schema t CastingDepartment
castingDepartmentSchema =
  FC.coerceSchema FC.boolean