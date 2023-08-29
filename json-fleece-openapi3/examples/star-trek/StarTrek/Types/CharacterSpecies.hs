{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterSpecies
  ( CharacterSpecies(..)
  , characterSpeciesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterSpecies.Denominator as Denominator
import qualified StarTrek.Types.CharacterSpecies.Name as Name
import qualified StarTrek.Types.CharacterSpecies.Numerator as Numerator
import qualified StarTrek.Types.CharacterSpecies.Uid as Uid

data CharacterSpecies = CharacterSpecies
  { denominator :: Maybe Denominator.Denominator -- ^ Denominator
  , uid :: Maybe Uid.Uid -- ^ Entity unique ID
  , name :: Maybe Name.Name -- ^ Species name
  , numerator :: Maybe Numerator.Numerator -- ^ Numerator
  }
  deriving (Eq, Show)

characterSpeciesSchema :: FC.Fleece schema => schema CharacterSpecies
characterSpeciesSchema =
  FC.object $
    FC.constructor CharacterSpecies
      #+ FC.optional "denominator" denominator Denominator.denominatorSchema
      #+ FC.optional "uid" uid Uid.uidSchema
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "numerator" numerator Numerator.numeratorSchema