{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalFullResponse
  ( AnimalFullResponse(..)
  , animalFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AnimalFull (AnimalFull, animalFullSchema)

data AnimalFullResponse = AnimalFullResponse
  { animal :: Maybe AnimalFull -- ^ Full animal, returned when queried using UID
  }
  deriving (Eq, Show)

animalFullResponseSchema :: FC.Fleece schema => schema AnimalFullResponse
animalFullResponseSchema =
  FC.object $
    FC.constructor AnimalFullResponse
      #+ FC.optional "animal" animal animalFullSchema