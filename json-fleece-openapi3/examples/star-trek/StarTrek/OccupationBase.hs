{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationBase
  ( OccupationBase(..)
  , occupationBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Bool, Eq, Maybe, Show)

data OccupationBase = OccupationBase
  { medicalOccupation :: Maybe Bool -- ^ Whether it's a medical occupation
  , name :: Text -- ^ Occupation name
  , uid :: Text -- ^ Occupation unique ID
  , legalOccupation :: Maybe Bool -- ^ Whether it's a legal occupation
  , scientificOccupation :: Maybe Bool -- ^ Whether it's a scientific occupation
  }
  deriving (Eq, Show)

occupationBaseSchema :: FC.Fleece schema => schema OccupationBase
occupationBaseSchema =
  FC.object $
    FC.constructor OccupationBase
      #+ FC.optional "medicalOccupation" medicalOccupation FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "legalOccupation" legalOccupation FC.boolean
      #+ FC.optional "scientificOccupation" scientificOccupation FC.boolean