{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.CameraAndElectricalDepartment
  ( CameraAndElectricalDepartment(..)
  , cameraAndElectricalDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CameraAndElectricalDepartment = CameraAndElectricalDepartment Bool
  deriving (Show, Eq)

cameraAndElectricalDepartmentSchema :: FC.Fleece schema => schema CameraAndElectricalDepartment
cameraAndElectricalDepartmentSchema =
  FC.coerceSchema FC.boolean