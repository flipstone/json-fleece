{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItem
  ( MinItemsOneInlineArrayOneOfItem(..)
  , minItemsOneInlineArrayOneOfItemSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItem.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItemItem as MinItemsOneInlineArrayOneOfItemItem

newtype MinItemsOneInlineArrayOneOfItem = MinItemsOneInlineArrayOneOfItem (NEL.NonEmpty MinItemsOneInlineArrayOneOfItemItem.MinItemsOneInlineArrayOneOfItemItem)
  deriving (Show, Eq)

minItemsOneInlineArrayOneOfItemSchema :: FC.Fleece t => FC.Schema t MinItemsOneInlineArrayOneOfItem
minItemsOneInlineArrayOneOfItemSchema =
  FC.coerceSchema (FC.nonEmpty MinItemsOneInlineArrayOneOfItemItem.minItemsOneInlineArrayOneOfItemItemSchema)