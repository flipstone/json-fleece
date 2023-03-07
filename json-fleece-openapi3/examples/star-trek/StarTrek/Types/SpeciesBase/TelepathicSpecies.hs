{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase.TelepathicSpecies
  ( TelepathicSpecies(..)
  , telepathicSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TelepathicSpecies = TelepathicSpecies Bool
  deriving (Show, Eq)

telepathicSpeciesSchema :: FC.Fleece schema => schema TelepathicSpecies
telepathicSpeciesSchema =
  FC.coerceSchema FC.boolean