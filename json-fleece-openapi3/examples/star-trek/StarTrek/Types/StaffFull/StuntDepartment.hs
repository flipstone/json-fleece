{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.StuntDepartment
  ( StuntDepartment(..)
  , stuntDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StuntDepartment = StuntDepartment Bool
  deriving (Show, Eq)

stuntDepartmentSchema :: FC.Fleece t => FC.Schema t StuntDepartment
stuntDepartmentSchema =
  FC.coerceSchema FC.boolean