{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodBase.Dessert
  ( Dessert(..)
  , dessertSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Dessert = Dessert Bool
  deriving (Show, Eq)

dessertSchema :: FC.Fleece schema => schema Dessert
dessertSchema =
  FC.coerceSchema FC.boolean