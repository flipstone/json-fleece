{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationFull
  ( OccupationFull(..)
  , occupationFullSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Bool, Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)

data OccupationFull = OccupationFull
  { medicalOccupation :: Maybe Bool -- ^ Whether it's a medical occupation
  , name :: Text -- ^ Occupation name
  , uid :: Text -- ^ Occupation unique ID
  , characters :: Maybe [CharacterBase] -- ^ Characters with this occupation
  , legalOccupation :: Maybe Bool -- ^ Whether it's a legal occupation
  , scientificOccupation :: Maybe Bool -- ^ Whether it's a scientific occupation
  }
  deriving (Eq, Show)

occupationFullSchema :: FC.Fleece schema => schema OccupationFull
occupationFullSchema =
  FC.object $
    FC.constructor OccupationFull
      #+ FC.optional "medicalOccupation" medicalOccupation FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "legalOccupation" legalOccupation FC.boolean
      #+ FC.optional "scientificOccupation" scientificOccupation FC.boolean