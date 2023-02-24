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
      #+ FC.optionalField FC.OmitKey_DelegateNull "numerator" numerator FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "denominator" denominator FC.integer