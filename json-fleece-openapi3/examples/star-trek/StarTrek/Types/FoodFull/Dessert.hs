{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodFull.Dessert
  ( Dessert(..)
  , dessertSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Dessert = Dessert Bool
  deriving (Show, Eq)

dessertSchema :: FC.Fleece t => FC.Schema t Dessert
dessertSchema =
  FC.coerceSchema FC.boolean