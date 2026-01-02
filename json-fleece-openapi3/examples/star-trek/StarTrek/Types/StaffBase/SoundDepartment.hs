{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.SoundDepartment
  ( SoundDepartment(..)
  , soundDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SoundDepartment = SoundDepartment Bool
  deriving (Show, Eq)

soundDepartmentSchema :: FC.Fleece t => FC.Schema t SoundDepartment
soundDepartmentSchema =
  FC.coerceSchema FC.boolean