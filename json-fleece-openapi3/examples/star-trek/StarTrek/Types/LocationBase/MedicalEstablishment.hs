{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.MedicalEstablishment
  ( MedicalEstablishment(..)
  , medicalEstablishmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MedicalEstablishment = MedicalEstablishment Bool
  deriving (Show, Eq)

medicalEstablishmentSchema :: FC.Fleece t => FC.Schema t MedicalEstablishment
medicalEstablishmentSchema =
  FC.coerceSchema FC.boolean