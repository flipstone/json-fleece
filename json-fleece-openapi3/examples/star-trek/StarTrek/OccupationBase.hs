{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationBase
  ( OccupationBase(..)
  , occupationBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.OccupationBase.LegalOccupation (LegalOccupation, legalOccupationSchema)
import StarTrek.OccupationBase.MedicalOccupation (MedicalOccupation, medicalOccupationSchema)
import StarTrek.OccupationBase.Name (Name, nameSchema)
import StarTrek.OccupationBase.ScientificOccupation (ScientificOccupation, scientificOccupationSchema)
import StarTrek.OccupationBase.Uid (Uid, uidSchema)

data OccupationBase = OccupationBase
  { medicalOccupation :: Maybe MedicalOccupation -- ^ Whether it's a medical occupation
  , name :: Name -- ^ Occupation name
  , uid :: Uid -- ^ Occupation unique ID
  , legalOccupation :: Maybe LegalOccupation -- ^ Whether it's a legal occupation
  , scientificOccupation :: Maybe ScientificOccupation -- ^ Whether it's a scientific occupation
  }
  deriving (Eq, Show)

occupationBaseSchema :: FC.Fleece schema => schema OccupationBase
occupationBaseSchema =
  FC.object $
    FC.constructor OccupationBase
      #+ FC.optional "medicalOccupation" medicalOccupation medicalOccupationSchema
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "legalOccupation" legalOccupation legalOccupationSchema
      #+ FC.optional "scientificOccupation" scientificOccupation scientificOccupationSchema