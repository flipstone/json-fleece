{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItem
  ( MinItemsOneInlineArrayNullableOneOfItem(..)
  , minItemsOneInlineArrayNullableOneOfItemSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItem.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItemItem as MinItemsOneInlineArrayNullableOneOfItemItem

newtype MinItemsOneInlineArrayNullableOneOfItem = MinItemsOneInlineArrayNullableOneOfItem (NEL.NonEmpty MinItemsOneInlineArrayNullableOneOfItemItem.MinItemsOneInlineArrayNullableOneOfItemItem)
  deriving (Show, Eq)

minItemsOneInlineArrayNullableOneOfItemSchema :: FC.Fleece t => FC.Schema t MinItemsOneInlineArrayNullableOneOfItem
minItemsOneInlineArrayNullableOneOfItemSchema =
  FC.coerceSchema (FC.nonEmpty MinItemsOneInlineArrayNullableOneOfItemItem.minItemsOneInlineArrayNullableOneOfItemItemSchema)