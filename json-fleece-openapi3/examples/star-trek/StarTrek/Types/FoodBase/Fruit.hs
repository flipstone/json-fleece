{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodBase.Fruit
  ( Fruit(..)
  , fruitSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Fruit = Fruit Bool
  deriving (Show, Eq)

fruitSchema :: FC.Fleece t => FC.Schema t Fruit
fruitSchema =
  FC.coerceSchema FC.boolean