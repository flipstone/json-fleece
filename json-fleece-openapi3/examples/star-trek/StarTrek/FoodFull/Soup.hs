{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodFull.Soup
  ( Soup(..)
  , soupSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Soup = Soup Bool
  deriving (Show, Eq)

soupSchema :: FC.Fleece schema => schema Soup
soupSchema =
  FC.coerceSchema FC.boolean