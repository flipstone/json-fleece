{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodFull.Beverage
  ( Beverage(..)
  , beverageSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Beverage = Beverage Bool
  deriving (Show, Eq)

beverageSchema :: FC.Fleece schema => schema Beverage
beverageSchema =
  FC.coerceSchema FC.boolean