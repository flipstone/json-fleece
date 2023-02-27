{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFull.ExtraGalacticSpecies
  ( ExtraGalacticSpecies(..)
  , extraGalacticSpeciesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ExtraGalacticSpecies = ExtraGalacticSpecies Bool
  deriving (Show, Eq)

extraGalacticSpeciesSchema :: FC.Fleece schema => schema ExtraGalacticSpecies
extraGalacticSpeciesSchema =
  FC.coerceSchema FC.boolean