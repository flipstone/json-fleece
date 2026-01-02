{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase.SpaceborneSpecies
  ( SpaceborneSpecies(..)
  , spaceborneSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SpaceborneSpecies = SpaceborneSpecies Bool
  deriving (Show, Eq)

spaceborneSpeciesSchema :: FC.Fleece t => FC.Schema t SpaceborneSpecies
spaceborneSpeciesSchema =
  FC.coerceSchema FC.boolean