{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionBaseResponse
  ( MedicalConditionBaseResponse(..)
  , medicalConditionBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MedicalConditionBase (MedicalConditionBase, medicalConditionBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MedicalConditionBaseResponse = MedicalConditionBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , medicalConditions :: Maybe [MedicalConditionBase] -- ^ List of medical conditions matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

medicalConditionBaseResponseSchema :: FC.Fleece schema => schema MedicalConditionBaseResponse
medicalConditionBaseResponseSchema =
  FC.object $
    FC.constructor MedicalConditionBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "medicalConditions" medicalConditions (FC.list medicalConditionBaseSchema)
      #+ FC.optional "page" page responsePageSchema