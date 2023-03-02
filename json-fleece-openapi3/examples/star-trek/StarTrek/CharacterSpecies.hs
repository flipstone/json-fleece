{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterSpecies
  ( CharacterSpecies(..)
  , characterSpeciesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CharacterSpecies.Denominator as Denominator
import qualified StarTrek.CharacterSpecies.Name as Name
import qualified StarTrek.CharacterSpecies.Numerator as Numerator
import qualified StarTrek.CharacterSpecies.Uid as Uid

data CharacterSpecies = CharacterSpecies
  { numerator :: Maybe Numerator.Numerator -- ^ Numerator
  , name :: Maybe Name.Name -- ^ Species name
  , uid :: Maybe Uid.Uid -- ^ Entity unique ID
  , denominator :: Maybe Denominator.Denominator -- ^ Denominator
  }
  deriving (Eq, Show)

characterSpeciesSchema :: FC.Fleece schema => schema CharacterSpecies
characterSpeciesSchema =
  FC.object $
    FC.constructor CharacterSpecies
      #+ FC.optional "numerator" numerator Numerator.numeratorSchema
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "denominator" denominator Denominator.denominatorSchema