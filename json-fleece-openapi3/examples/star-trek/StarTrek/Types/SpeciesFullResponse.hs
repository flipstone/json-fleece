{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesFullResponse
  ( SpeciesFullResponse(..)
  , speciesFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SpeciesFull as SpeciesFull

data SpeciesFullResponse = SpeciesFullResponse
  { species :: Maybe SpeciesFull.SpeciesFull -- ^ Full species, returned when queried using UID
  }
  deriving (Eq, Show)

speciesFullResponseSchema :: FC.Fleece t => FC.Schema t SpeciesFullResponse
speciesFullResponseSchema =
  FC.object $
    FC.constructor SpeciesFullResponse
      #+ FC.optional "species" species SpeciesFull.speciesFullSchema