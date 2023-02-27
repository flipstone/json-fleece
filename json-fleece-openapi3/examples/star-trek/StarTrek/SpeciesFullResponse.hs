{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesFullResponse
  ( SpeciesFullResponse(..)
  , speciesFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SpeciesFull (SpeciesFull, speciesFullSchema)

data SpeciesFullResponse = SpeciesFullResponse
  { species :: Maybe SpeciesFull -- ^ Full species, returned when queried using UID
  }
  deriving (Eq, Show)

speciesFullResponseSchema :: FC.Fleece schema => schema SpeciesFullResponse
speciesFullResponseSchema =
  FC.object $
    FC.constructor SpeciesFullResponse
      #+ FC.optional "species" species speciesFullSchema