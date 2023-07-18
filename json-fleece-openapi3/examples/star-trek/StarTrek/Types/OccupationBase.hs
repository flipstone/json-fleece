{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OccupationBase
  ( OccupationBase(..)
  , occupationBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OccupationBase.LegalOccupation as LegalOccupation
import qualified StarTrek.Types.OccupationBase.MedicalOccupation as MedicalOccupation
import qualified StarTrek.Types.OccupationBase.Name as Name
import qualified StarTrek.Types.OccupationBase.ScientificOccupation as ScientificOccupation
import qualified StarTrek.Types.OccupationBase.Uid as Uid

data OccupationBase = OccupationBase
  { medicalOccupation :: Maybe MedicalOccupation.MedicalOccupation -- ^ Whether it's a medical occupation
  , uid :: Uid.Uid -- ^ Occupation unique ID
  , scientificOccupation :: Maybe ScientificOccupation.ScientificOccupation -- ^ Whether it's a scientific occupation
  , legalOccupation :: Maybe LegalOccupation.LegalOccupation -- ^ Whether it's a legal occupation
  , name :: Name.Name -- ^ Occupation name
  }
  deriving (Eq, Show)

occupationBaseSchema :: FC.Fleece schema => schema OccupationBase
occupationBaseSchema =
  FC.object $
    FC.constructor OccupationBase
      #+ FC.optional "medicalOccupation" medicalOccupation MedicalOccupation.medicalOccupationSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "scientificOccupation" scientificOccupation ScientificOccupation.scientificOccupationSchema
      #+ FC.optional "legalOccupation" legalOccupation LegalOccupation.legalOccupationSchema
      #+ FC.required "name" name Name.nameSchema