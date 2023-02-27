{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodBase.Fruit
  ( Fruit(..)
  , fruitSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Fruit = Fruit Bool
  deriving (Show, Eq)

fruitSchema :: FC.Fleece schema => schema Fruit
fruitSchema =
  FC.coerceSchema FC.boolean