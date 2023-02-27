{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterSpecies
  ( CharacterSpecies(..)
  , characterSpeciesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterSpecies.Denominator (Denominator, denominatorSchema)
import StarTrek.CharacterSpecies.Name (Name, nameSchema)
import StarTrek.CharacterSpecies.Numerator (Numerator, numeratorSchema)
import StarTrek.CharacterSpecies.Uid (Uid, uidSchema)

data CharacterSpecies = CharacterSpecies
  { numerator :: Maybe Numerator -- ^ Numerator
  , name :: Maybe Name -- ^ Species name
  , uid :: Maybe Uid -- ^ Entity unique ID
  , denominator :: Maybe Denominator -- ^ Denominator
  }
  deriving (Eq, Show)

characterSpeciesSchema :: FC.Fleece schema => schema CharacterSpecies
characterSpeciesSchema =
  FC.object $
    FC.constructor CharacterSpecies
      #+ FC.optional "numerator" numerator numeratorSchema
      #+ FC.optional "name" name nameSchema
      #+ FC.optional "uid" uid uidSchema
      #+ FC.optional "denominator" denominator denominatorSchema