{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationFull
  ( OccupationFull(..)
  , occupationFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.OccupationFull.LegalOccupation (LegalOccupation, legalOccupationSchema)
import StarTrek.OccupationFull.MedicalOccupation (MedicalOccupation, medicalOccupationSchema)
import StarTrek.OccupationFull.Name (Name, nameSchema)
import StarTrek.OccupationFull.ScientificOccupation (ScientificOccupation, scientificOccupationSchema)
import StarTrek.OccupationFull.Uid (Uid, uidSchema)

data OccupationFull = OccupationFull
  { medicalOccupation :: Maybe MedicalOccupation -- ^ Whether it's a medical occupation
  , name :: Name -- ^ Occupation name
  , uid :: Uid -- ^ Occupation unique ID
  , characters :: Maybe [CharacterBase] -- ^ Base character, returned in search results
  , legalOccupation :: Maybe LegalOccupation -- ^ Whether it's a legal occupation
  , scientificOccupation :: Maybe ScientificOccupation -- ^ Whether it's a scientific occupation
  }
  deriving (Eq, Show)

occupationFullSchema :: FC.Fleece schema => schema OccupationFull
occupationFullSchema =
  FC.object $
    FC.constructor OccupationFull
      #+ FC.optional "medicalOccupation" medicalOccupation medicalOccupationSchema
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "legalOccupation" legalOccupation legalOccupationSchema
      #+ FC.optional "scientificOccupation" scientificOccupation scientificOccupationSchema