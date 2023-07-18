{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OccupationFull
  ( OccupationFull(..)
  , occupationFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.OccupationFull.LegalOccupation as LegalOccupation
import qualified StarTrek.Types.OccupationFull.MedicalOccupation as MedicalOccupation
import qualified StarTrek.Types.OccupationFull.Name as Name
import qualified StarTrek.Types.OccupationFull.ScientificOccupation as ScientificOccupation
import qualified StarTrek.Types.OccupationFull.Uid as Uid

data OccupationFull = OccupationFull
  { medicalOccupation :: Maybe MedicalOccupation.MedicalOccupation -- ^ Whether it's a medical occupation
  , characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , uid :: Uid.Uid -- ^ Occupation unique ID
  , scientificOccupation :: Maybe ScientificOccupation.ScientificOccupation -- ^ Whether it's a scientific occupation
  , legalOccupation :: Maybe LegalOccupation.LegalOccupation -- ^ Whether it's a legal occupation
  , name :: Name.Name -- ^ Occupation name
  }
  deriving (Eq, Show)

occupationFullSchema :: FC.Fleece schema => schema OccupationFull
occupationFullSchema =
  FC.object $
    FC.constructor OccupationFull
      #+ FC.optional "medicalOccupation" medicalOccupation MedicalOccupation.medicalOccupationSchema
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "scientificOccupation" scientificOccupation ScientificOccupation.scientificOccupationSchema
      #+ FC.optional "legalOccupation" legalOccupation LegalOccupation.legalOccupationSchema
      #+ FC.required "name" name Name.nameSchema