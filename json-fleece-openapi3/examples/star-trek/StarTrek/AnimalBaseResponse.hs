{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBaseResponse
  ( AnimalBaseResponse(..)
  , animalBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.AnimalBase (AnimalBase, animalBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data AnimalBaseResponse = AnimalBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , animals :: Maybe [AnimalBase] -- ^ List of animals matching given criteria
  }
  deriving (Eq, Show)

animalBaseResponseSchema :: FC.Fleece schema => schema AnimalBaseResponse
animalBaseResponseSchema =
  FC.object $
    FC.constructor AnimalBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "animals" animals (FC.list animalBaseSchema)