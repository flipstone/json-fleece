{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodFull.Juice
  ( Juice(..)
  , juiceSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Juice = Juice Bool
  deriving (Show, Eq)

juiceSchema :: FC.Fleece t => FC.Schema t Juice
juiceSchema =
  FC.coerceSchema FC.boolean