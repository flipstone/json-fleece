{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterSpecies
  ( CharacterSpecies(..)
  , characterSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)

data CharacterSpecies = CharacterSpecies
  { numerator :: Maybe Integer -- ^ Numerator
  , name :: Maybe Text -- ^ Species name
  , uid :: Maybe Text -- ^ Entity unique ID
  , denominator :: Maybe Integer -- ^ Denominator
  }
  deriving (Eq, Show)

characterSpeciesSchema :: FC.Fleece schema => schema CharacterSpecies
characterSpeciesSchema =
  FC.object $
    FC.constructor CharacterSpecies
      #+ FC.optional "numerator" numerator FC.integer
      #+ FC.optional "name" name FC.text
      #+ FC.optional "uid" uid FC.text
      #+ FC.optional "denominator" denominator FC.integer