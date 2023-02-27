{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBaseResponse
  ( SpeciesBaseResponse(..)
  , speciesBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.SpeciesBase (SpeciesBase, speciesBaseSchema)

data SpeciesBaseResponse = SpeciesBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , species :: Maybe [SpeciesBase] -- ^ List of species matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

speciesBaseResponseSchema :: FC.Fleece schema => schema SpeciesBaseResponse
speciesBaseResponseSchema =
  FC.object $
    FC.constructor SpeciesBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "species" species (FC.list speciesBaseSchema)
      #+ FC.optional "page" page responsePageSchema