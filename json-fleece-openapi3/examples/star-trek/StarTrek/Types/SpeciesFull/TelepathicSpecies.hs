{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesFull.TelepathicSpecies
  ( TelepathicSpecies(..)
  , telepathicSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TelepathicSpecies = TelepathicSpecies Bool
  deriving (Show, Eq)

telepathicSpeciesSchema :: FC.Fleece t => FC.Schema t TelepathicSpecies
telepathicSpeciesSchema =
  FC.coerceSchema FC.boolean