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
  , name :: Maybe Name.Name -- ^ Species name
  , numerator :: Maybe Numerator.Numerator -- ^ Numerator
  , uid :: Maybe Uid.Uid -- ^ Entity unique ID
  }
  deriving (Eq, Show)

characterSpeciesSchema :: FC.Fleece schema => schema CharacterSpecies
characterSpeciesSchema =
  FC.object $
    FC.constructor CharacterSpecies
      #+ FC.optional "denominator" denominator Denominator.denominatorSchema
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "numerator" numerator Numerator.numeratorSchema
      #+ FC.optional "uid" uid Uid.uidSchema