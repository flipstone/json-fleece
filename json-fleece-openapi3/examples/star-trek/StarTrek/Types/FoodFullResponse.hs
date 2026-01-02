{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodFullResponse
  ( FoodFullResponse(..)
  , foodFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.FoodFull as FoodFull

data FoodFullResponse = FoodFullResponse
  { food :: Maybe FoodFull.FoodFull -- ^ Full food, returned when queried using UID
  }
  deriving (Eq, Show)

foodFullResponseSchema :: FC.Fleece t => FC.Schema t FoodFullResponse
foodFullResponseSchema =
  FC.object $
    FC.constructor FoodFullResponse
      #+ FC.optional "food" food FoodFull.foodFullSchema