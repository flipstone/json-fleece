{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesFull.NonCorporealSpecies
  ( NonCorporealSpecies(..)
  , nonCorporealSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype NonCorporealSpecies = NonCorporealSpecies Bool
  deriving (Show, Eq)

nonCorporealSpeciesSchema :: FC.Fleece schema => schema NonCorporealSpecies
nonCorporealSpeciesSchema =
  FC.coerceSchema FC.boolean