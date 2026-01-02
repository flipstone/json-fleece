{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.MusicDepartment
  ( MusicDepartment(..)
  , musicDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MusicDepartment = MusicDepartment Bool
  deriving (Show, Eq)

musicDepartmentSchema :: FC.Fleece t => FC.Schema t MusicDepartment
musicDepartmentSchema =
  FC.coerceSchema FC.boolean