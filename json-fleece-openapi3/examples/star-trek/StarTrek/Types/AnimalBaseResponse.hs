{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalBaseResponse
  ( AnimalBaseResponse(..)
  , animalBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.AnimalBase as AnimalBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data AnimalBaseResponse = AnimalBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , animals :: Maybe [AnimalBase.AnimalBase] -- ^ Base animal, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

animalBaseResponseSchema :: FC.Fleece schema => schema AnimalBaseResponse
animalBaseResponseSchema =
  FC.object $
    FC.constructor AnimalBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "animals" animals (FC.list AnimalBase.animalBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema