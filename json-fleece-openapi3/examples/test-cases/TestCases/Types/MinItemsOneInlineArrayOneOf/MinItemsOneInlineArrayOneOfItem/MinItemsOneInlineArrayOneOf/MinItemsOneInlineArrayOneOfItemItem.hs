{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItem.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItemItem
  ( MinItemsOneInlineArrayOneOfItemItem(..)
  , minItemsOneInlineArrayOneOfItemItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MinItemsOneInlineArrayOneOfItemItem = MinItemsOneInlineArrayOneOfItemItem Bool
  deriving (Show, Eq)

minItemsOneInlineArrayOneOfItemItemSchema :: FC.Fleece schema => schema MinItemsOneInlineArrayOneOfItemItem
minItemsOneInlineArrayOneOfItemItemSchema =
  FC.coerceSchema FC.boolean