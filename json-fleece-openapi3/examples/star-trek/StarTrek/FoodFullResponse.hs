{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodFullResponse
  ( FoodFullResponse(..)
  , foodFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.FoodFull (FoodFull, foodFullSchema)

data FoodFullResponse = FoodFullResponse
  { food :: Maybe FoodFull -- ^ Full food, returned when queried using UID
  }
  deriving (Eq, Show)

foodFullResponseSchema :: FC.Fleece schema => schema FoodFullResponse
foodFullResponseSchema =
  FC.object $
    FC.constructor FoodFullResponse
      #+ FC.optional "food" food foodFullSchema