{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBaseResponse
  ( AnimalBaseResponse(..)
  , animalBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "animals" animals (FC.list animalBaseSchema)