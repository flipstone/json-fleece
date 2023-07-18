{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MedicalConditionBase
  ( MedicalConditionBase(..)
  , medicalConditionBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MedicalConditionBase.Name as Name
import qualified StarTrek.Types.MedicalConditionBase.PsychologicalCondition as PsychologicalCondition
import qualified StarTrek.Types.MedicalConditionBase.Uid as Uid

data MedicalConditionBase = MedicalConditionBase
  { uid :: Uid.Uid -- ^ Medical condition unique ID
  , psychologicalCondition :: Maybe PsychologicalCondition.PsychologicalCondition -- ^ Whether it's a psychological condition
  , name :: Name.Name -- ^ Medical condition name
  }
  deriving (Eq, Show)

medicalConditionBaseSchema :: FC.Fleece schema => schema MedicalConditionBase
medicalConditionBaseSchema =
  FC.object $
    FC.constructor MedicalConditionBase
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "psychologicalCondition" psychologicalCondition PsychologicalCondition.psychologicalConditionSchema
      #+ FC.required "name" name Name.nameSchema