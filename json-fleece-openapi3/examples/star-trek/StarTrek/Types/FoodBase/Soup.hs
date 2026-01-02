{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodBase.Soup
  ( Soup(..)
  , soupSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Soup = Soup Bool
  deriving (Show, Eq)

soupSchema :: FC.Fleece t => FC.Schema t Soup
soupSchema =
  FC.coerceSchema FC.boolean