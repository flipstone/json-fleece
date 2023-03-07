{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MedicalConditionBaseResponse
  ( MedicalConditionBaseResponse(..)
  , medicalConditionBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MedicalConditionBase as MedicalConditionBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data MedicalConditionBaseResponse = MedicalConditionBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , medicalConditions :: Maybe [MedicalConditionBase.MedicalConditionBase] -- ^ Base medical condition, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

medicalConditionBaseResponseSchema :: FC.Fleece schema => schema MedicalConditionBaseResponse
medicalConditionBaseResponseSchema =
  FC.object $
    FC.constructor MedicalConditionBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "medicalConditions" medicalConditions (FC.list MedicalConditionBase.medicalConditionBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema