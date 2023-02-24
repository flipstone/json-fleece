{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodBaseResponse
  ( FoodBaseResponse(..)
  , foodBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.FoodBase (FoodBase, foodBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data FoodBaseResponse = FoodBaseResponse
  { foods :: Maybe [FoodBase] -- ^ List of foods matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

foodBaseResponseSchema :: FC.Fleece schema => schema FoodBaseResponse
foodBaseResponseSchema =
  FC.object $
    FC.constructor FoodBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "foods" foods (FC.list foodBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema