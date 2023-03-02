{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationBase
  ( OccupationBase(..)
  , occupationBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.OccupationBase.LegalOccupation as LegalOccupation
import qualified StarTrek.OccupationBase.MedicalOccupation as MedicalOccupation
import qualified StarTrek.OccupationBase.Name as Name
import qualified StarTrek.OccupationBase.ScientificOccupation as ScientificOccupation
import qualified StarTrek.OccupationBase.Uid as Uid

data OccupationBase = OccupationBase
  { medicalOccupation :: Maybe MedicalOccupation.MedicalOccupation -- ^ Whether it's a medical occupation
  , name :: Name.Name -- ^ Occupation name
  , uid :: Uid.Uid -- ^ Occupation unique ID
  , legalOccupation :: Maybe LegalOccupation.LegalOccupation -- ^ Whether it's a legal occupation
  , scientificOccupation :: Maybe ScientificOccupation.ScientificOccupation -- ^ Whether it's a scientific occupation
  }
  deriving (Eq, Show)

occupationBaseSchema :: FC.Fleece schema => schema OccupationBase
occupationBaseSchema =
  FC.object $
    FC.constructor OccupationBase
      #+ FC.optional "medicalOccupation" medicalOccupation MedicalOccupation.medicalOccupationSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "legalOccupation" legalOccupation LegalOccupation.legalOccupationSchema
      #+ FC.optional "scientificOccupation" scientificOccupation ScientificOccupation.scientificOccupationSchema