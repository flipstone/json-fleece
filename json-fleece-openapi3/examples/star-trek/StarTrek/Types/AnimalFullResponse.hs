{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalFullResponse
  ( AnimalFullResponse(..)
  , animalFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AnimalFull as AnimalFull

data AnimalFullResponse = AnimalFullResponse
  { animal :: Maybe AnimalFull.AnimalFull -- ^ Full animal, returned when queried using UID
  }
  deriving (Eq, Show)

animalFullResponseSchema :: FC.Fleece t => FC.Schema t AnimalFullResponse
animalFullResponseSchema =
  FC.object $
    FC.constructor AnimalFullResponse
      #+ FC.optional "animal" animal AnimalFull.animalFullSchema