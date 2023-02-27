{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionBase
  ( MedicalConditionBase(..)
  , medicalConditionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MedicalConditionBase.Name (Name, nameSchema)
import StarTrek.MedicalConditionBase.PsychologicalCondition (PsychologicalCondition, psychologicalConditionSchema)
import StarTrek.MedicalConditionBase.Uid (Uid, uidSchema)

data MedicalConditionBase = MedicalConditionBase
  { name :: Name -- ^ Medical condition name
  , uid :: Uid -- ^ Medical condition unique ID
  , psychologicalCondition :: Maybe PsychologicalCondition -- ^ Whether it's a psychological condition
  }
  deriving (Eq, Show)

medicalConditionBaseSchema :: FC.Fleece schema => schema MedicalConditionBase
medicalConditionBaseSchema =
  FC.object $
    FC.constructor MedicalConditionBase
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "psychologicalCondition" psychologicalCondition psychologicalConditionSchema