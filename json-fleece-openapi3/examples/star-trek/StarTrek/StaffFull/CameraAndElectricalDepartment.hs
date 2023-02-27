{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.CameraAndElectricalDepartment
  ( CameraAndElectricalDepartment(..)
  , cameraAndElectricalDepartmentSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CameraAndElectricalDepartment = CameraAndElectricalDepartment Bool
  deriving (Show, Eq)

cameraAndElectricalDepartmentSchema :: FC.Fleece schema => schema CameraAndElectricalDepartment
cameraAndElectricalDepartmentSchema =
  FC.coerceSchema FC.boolean