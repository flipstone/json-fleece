{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MedicalConditionFull
  ( MedicalConditionFull(..)
  , medicalConditionFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MedicalConditionFull.Name as Name
import qualified StarTrek.Types.MedicalConditionFull.PsychologicalCondition as PsychologicalCondition
import qualified StarTrek.Types.MedicalConditionFull.Uid as Uid

data MedicalConditionFull = MedicalConditionFull
  { uid :: Uid.Uid -- ^ Medical condition unique ID
  , name :: Name.Name -- ^ Medical condition name
  , psychologicalCondition :: Maybe PsychologicalCondition.PsychologicalCondition -- ^ Whether it's a psychological condition
  }
  deriving (Eq, Show)

medicalConditionFullSchema :: FC.Fleece schema => schema MedicalConditionFull
medicalConditionFullSchema =
  FC.object $
    FC.constructor MedicalConditionFull
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "psychologicalCondition" psychologicalCondition PsychologicalCondition.psychologicalConditionSchema