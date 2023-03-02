{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodFullResponse
  ( FoodFullResponse(..)
  , foodFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.FoodFull as FoodFull

data FoodFullResponse = FoodFullResponse
  { food :: Maybe FoodFull.FoodFull -- ^ Full food, returned when queried using UID
  }
  deriving (Eq, Show)

foodFullResponseSchema :: FC.Fleece schema => schema FoodFullResponse
foodFullResponseSchema =
  FC.object $
    FC.constructor FoodFullResponse
      #+ FC.optional "food" food FoodFull.foodFullSchema