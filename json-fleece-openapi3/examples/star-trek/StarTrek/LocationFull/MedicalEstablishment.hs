{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.MedicalEstablishment
  ( MedicalEstablishment(..)
  , medicalEstablishmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MedicalEstablishment = MedicalEstablishment Bool
  deriving (Show, Eq)

medicalEstablishmentSchema :: FC.Fleece schema => schema MedicalEstablishment
medicalEstablishmentSchema =
  FC.coerceSchema FC.boolean