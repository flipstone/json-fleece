{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.MedicalEquipment
  ( MedicalEquipment(..)
  , medicalEquipmentSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MedicalEquipment = MedicalEquipment Bool
  deriving (Show, Eq)

medicalEquipmentSchema :: FC.Fleece schema => schema MedicalEquipment
medicalEquipmentSchema =
  FC.coerceSchema FC.boolean