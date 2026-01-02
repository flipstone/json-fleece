{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.CameraAndElectricalDepartment
  ( CameraAndElectricalDepartment(..)
  , cameraAndElectricalDepartmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CameraAndElectricalDepartment = CameraAndElectricalDepartment Bool
  deriving (Show, Eq)

cameraAndElectricalDepartmentSchema :: FC.Fleece t => FC.Schema t CameraAndElectricalDepartment
cameraAndElectricalDepartmentSchema =
  FC.coerceSchema FC.boolean