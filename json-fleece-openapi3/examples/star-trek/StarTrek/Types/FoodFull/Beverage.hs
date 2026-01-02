{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodFull.Beverage
  ( Beverage(..)
  , beverageSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Beverage = Beverage Bool
  deriving (Show, Eq)

beverageSchema :: FC.Fleece t => FC.Schema t Beverage
beverageSchema =
  FC.coerceSchema FC.boolean