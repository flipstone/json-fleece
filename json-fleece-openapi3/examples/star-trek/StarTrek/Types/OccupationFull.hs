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
  { characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , legalOccupation :: Maybe LegalOccupation.LegalOccupation -- ^ Whether it's a legal occupation
  , medicalOccupation :: Maybe MedicalOccupation.MedicalOccupation -- ^ Whether it's a medical occupation
  , name :: Name.Name -- ^ Occupation name
  , scientificOccupation :: Maybe ScientificOccupation.ScientificOccupation -- ^ Whether it's a scientific occupation
  , uid :: Uid.Uid -- ^ Occupation unique ID
  }
  deriving (Eq, Show)

occupationFullSchema :: FC.Fleece t => FC.Schema t OccupationFull
occupationFullSchema =
  FC.object $
    FC.constructor OccupationFull
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "legalOccupation" legalOccupation LegalOccupation.legalOccupationSchema
      #+ FC.optional "medicalOccupation" medicalOccupation MedicalOccupation.medicalOccupationSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "scientificOccupation" scientificOccupation ScientificOccupation.scientificOccupationSchema
      #+ FC.required "uid" uid Uid.uidSchema