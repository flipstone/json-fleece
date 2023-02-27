{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBase.TelepathicSpecies
  ( TelepathicSpecies(..)
  , telepathicSpeciesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TelepathicSpecies = TelepathicSpecies Bool
  deriving (Show, Eq)

telepathicSpeciesSchema :: FC.Fleece schema => schema TelepathicSpecies
telepathicSpeciesSchema =
  FC.coerceSchema FC.boolean