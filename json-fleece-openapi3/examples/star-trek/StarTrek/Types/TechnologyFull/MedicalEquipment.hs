{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.MedicalEquipment
  ( MedicalEquipment(..)
  , medicalEquipmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MedicalEquipment = MedicalEquipment Bool
  deriving (Show, Eq)

medicalEquipmentSchema :: FC.Fleece t => FC.Schema t MedicalEquipment
medicalEquipmentSchema =
  FC.coerceSchema FC.boolean