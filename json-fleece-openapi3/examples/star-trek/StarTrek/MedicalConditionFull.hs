{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionFull
  ( MedicalConditionFull(..)
  , medicalConditionFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data MedicalConditionFull = MedicalConditionFull
  { name :: Text -- ^ Medical condition name
  , uid :: Text -- ^ Medical condition unique ID
  , psychologicalCondition :: Maybe Bool -- ^ Whether it's a psychological condition
  }
  deriving (Eq, Show)

medicalConditionFullSchema :: FC.Fleece schema => schema MedicalConditionFull
medicalConditionFullSchema =
  FC.object $
    FC.constructor MedicalConditionFull
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "psychologicalCondition" psychologicalCondition FC.boolean