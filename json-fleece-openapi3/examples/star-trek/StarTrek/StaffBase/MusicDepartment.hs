{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.MusicDepartment
  ( MusicDepartment(..)
  , musicDepartmentSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MusicDepartment = MusicDepartment Bool
  deriving (Show, Eq)

musicDepartmentSchema :: FC.Fleece schema => schema MusicDepartment
musicDepartmentSchema =
  FC.coerceSchema FC.boolean