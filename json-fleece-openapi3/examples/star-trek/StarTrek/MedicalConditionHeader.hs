{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MedicalConditionHeader
  ( MedicalConditionHeader(..)
  , medicalConditionHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data MedicalConditionHeader = MedicalConditionHeader
  { name :: Text -- ^ Medical condition name
  , uid :: Text -- ^ Medical condition unique ID
  }
  deriving (Eq, Show)

medicalConditionHeaderSchema :: FC.Fleece schema => schema MedicalConditionHeader
medicalConditionHeaderSchema =
  FC.object $
    FC.constructor MedicalConditionHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text