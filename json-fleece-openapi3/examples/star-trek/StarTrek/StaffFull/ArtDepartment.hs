{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.ArtDepartment
  ( ArtDepartment(..)
  , artDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ArtDepartment = ArtDepartment Bool
  deriving (Show, Eq)

artDepartmentSchema :: FC.Fleece schema => schema ArtDepartment
artDepartmentSchema =
  FC.coerceSchema FC.boolean