{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionFullResponse
  ( MedicalConditionFullResponse(..)
  , medicalConditionFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MedicalConditionFull (MedicalConditionFull, medicalConditionFullSchema)

data MedicalConditionFullResponse = MedicalConditionFullResponse
  { medicalCondition :: Maybe MedicalConditionFull -- ^ Full medical condition, returned when queried using UID
  }
  deriving (Eq, Show)

medicalConditionFullResponseSchema :: FC.Fleece schema => schema MedicalConditionFullResponse
medicalConditionFullResponseSchema =
  FC.object $
    FC.constructor MedicalConditionFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "medicalCondition" medicalCondition medicalConditionFullSchema