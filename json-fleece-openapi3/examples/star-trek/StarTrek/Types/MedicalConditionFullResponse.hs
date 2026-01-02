{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MedicalConditionFullResponse
  ( MedicalConditionFullResponse(..)
  , medicalConditionFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MedicalConditionFull as MedicalConditionFull

data MedicalConditionFullResponse = MedicalConditionFullResponse
  { medicalCondition :: Maybe MedicalConditionFull.MedicalConditionFull -- ^ Full medical condition, returned when queried using UID
  }
  deriving (Eq, Show)

medicalConditionFullResponseSchema :: FC.Fleece t => FC.Schema t MedicalConditionFullResponse
medicalConditionFullResponseSchema =
  FC.object $
    FC.constructor MedicalConditionFullResponse
      #+ FC.optional "medicalCondition" medicalCondition MedicalConditionFull.medicalConditionFullSchema