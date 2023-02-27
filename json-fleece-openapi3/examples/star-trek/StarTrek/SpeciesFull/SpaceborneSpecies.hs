{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFull.SpaceborneSpecies
  ( SpaceborneSpecies(..)
  , spaceborneSpeciesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SpaceborneSpecies = SpaceborneSpecies Bool
  deriving (Show, Eq)

spaceborneSpeciesSchema :: FC.Fleece schema => schema SpaceborneSpecies
spaceborneSpeciesSchema =
  FC.coerceSchema FC.boolean