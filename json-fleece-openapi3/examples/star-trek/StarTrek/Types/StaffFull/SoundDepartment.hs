{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.SoundDepartment
  ( SoundDepartment(..)
  , soundDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SoundDepartment = SoundDepartment Bool
  deriving (Show, Eq)

soundDepartmentSchema :: FC.Fleece schema => schema SoundDepartment
soundDepartmentSchema =
  FC.coerceSchema FC.boolean