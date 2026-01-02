{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesFull.UnnamedSpecies
  ( UnnamedSpecies(..)
  , unnamedSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype UnnamedSpecies = UnnamedSpecies Bool
  deriving (Show, Eq)

unnamedSpeciesSchema :: FC.Fleece t => FC.Schema t UnnamedSpecies
unnamedSpeciesSchema =
  FC.coerceSchema FC.boolean