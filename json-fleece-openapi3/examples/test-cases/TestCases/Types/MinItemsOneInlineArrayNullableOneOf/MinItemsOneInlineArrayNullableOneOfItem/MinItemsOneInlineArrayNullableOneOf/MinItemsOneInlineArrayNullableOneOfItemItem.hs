{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItem.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItemItem
  ( MinItemsOneInlineArrayNullableOneOfItemItem(..)
  , minItemsOneInlineArrayNullableOneOfItemItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MinItemsOneInlineArrayNullableOneOfItemItem = MinItemsOneInlineArrayNullableOneOfItemItem Bool
  deriving (Show, Eq)

minItemsOneInlineArrayNullableOneOfItemItemSchema :: FC.Fleece schema => schema MinItemsOneInlineArrayNullableOneOfItemItem
minItemsOneInlineArrayNullableOneOfItemItemSchema =
  FC.coerceSchema FC.boolean