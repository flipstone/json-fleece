{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionBase
  ( MedicalConditionBase(..)
  , medicalConditionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.MedicalConditionBase.Name as Name
import qualified StarTrek.MedicalConditionBase.PsychologicalCondition as PsychologicalCondition
import qualified StarTrek.MedicalConditionBase.Uid as Uid

data MedicalConditionBase = MedicalConditionBase
  { name :: Name.Name -- ^ Medical condition name
  , uid :: Uid.Uid -- ^ Medical condition unique ID
  , psychologicalCondition :: Maybe PsychologicalCondition.PsychologicalCondition -- ^ Whether it's a psychological condition
  }
  deriving (Eq, Show)

medicalConditionBaseSchema :: FC.Fleece schema => schema MedicalConditionBase
medicalConditionBaseSchema =
  FC.object $
    FC.constructor MedicalConditionBase
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "psychologicalCondition" psychologicalCondition PsychologicalCondition.psychologicalConditionSchema