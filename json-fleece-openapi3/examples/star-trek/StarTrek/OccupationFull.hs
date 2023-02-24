{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationFull
  ( OccupationFull(..)
  , occupationFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "medicalOccupation" medicalOccupation FC.boolean
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "legalOccupation" legalOccupation FC.boolean
      #+ FC.optionalField FC.OmitKey_DelegateNull "scientificOccupation" scientificOccupation FC.boolean