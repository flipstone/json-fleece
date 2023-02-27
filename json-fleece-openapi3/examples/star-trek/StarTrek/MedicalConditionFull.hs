{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionFull
  ( MedicalConditionFull(..)
  , medicalConditionFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MedicalConditionFull.Name (Name, nameSchema)
import StarTrek.MedicalConditionFull.PsychologicalCondition (PsychologicalCondition, psychologicalConditionSchema)
import StarTrek.MedicalConditionFull.Uid (Uid, uidSchema)

data MedicalConditionFull = MedicalConditionFull
  { name :: Name -- ^ Medical condition name
  , uid :: Uid -- ^ Medical condition unique ID
  , psychologicalCondition :: Maybe PsychologicalCondition -- ^ Whether it's a psychological condition
  }
  deriving (Eq, Show)

medicalConditionFullSchema :: FC.Fleece schema => schema MedicalConditionFull
medicalConditionFullSchema =
  FC.object $
    FC.constructor MedicalConditionFull
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "psychologicalCondition" psychologicalCondition psychologicalConditionSchema