{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodBaseResponse
  ( FoodBaseResponse(..)
  , foodBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.FoodBase as FoodBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data FoodBaseResponse = FoodBaseResponse
  { foods :: Maybe [FoodBase.FoodBase] -- ^ Base food, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

foodBaseResponseSchema :: FC.Fleece schema => schema FoodBaseResponse
foodBaseResponseSchema =
  FC.object $
    FC.constructor FoodBaseResponse
      #+ FC.optional "foods" foods (FC.list FoodBase.foodBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema