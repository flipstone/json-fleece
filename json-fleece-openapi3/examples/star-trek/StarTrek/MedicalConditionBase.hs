{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionBase
  ( MedicalConditionBase(..)
  , medicalConditionBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data MedicalConditionBase = MedicalConditionBase
  { name :: Text -- ^ Medical condition name
  , uid :: Text -- ^ Medical condition unique ID
  , psychologicalCondition :: Maybe Bool -- ^ Whether it's a psychological condition
  }
  deriving (Eq, Show)

medicalConditionBaseSchema :: FC.Fleece schema => schema MedicalConditionBase
medicalConditionBaseSchema =
  FC.object $
    FC.constructor MedicalConditionBase
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "psychologicalCondition" psychologicalCondition FC.boolean