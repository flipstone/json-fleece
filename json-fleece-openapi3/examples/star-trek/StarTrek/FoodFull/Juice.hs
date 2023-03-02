{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodFull.Juice
  ( Juice(..)
  , juiceSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Juice = Juice Bool
  deriving (Show, Eq)

juiceSchema :: FC.Fleece schema => schema Juice
juiceSchema =
  FC.coerceSchema FC.boolean