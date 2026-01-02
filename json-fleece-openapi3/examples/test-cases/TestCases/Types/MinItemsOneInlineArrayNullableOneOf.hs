{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineArrayNullableOneOf
  ( MinItemsOneInlineArrayNullableOneOf(..)
  , minItemsOneInlineArrayNullableOneOfSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.MinItemsOneInlineArrayNullableOneOf.MinItemsOneInlineArrayNullableOneOfItem as MinItemsOneInlineArrayNullableOneOfItem

newtype MinItemsOneInlineArrayNullableOneOf = MinItemsOneInlineArrayNullableOneOf (NEL.NonEmpty MinItemsOneInlineArrayNullableOneOfItem.MinItemsOneInlineArrayNullableOneOfItem)
  deriving (Show, Eq)

minItemsOneInlineArrayNullableOneOfSchema :: FC.Fleece t => FC.Schema t MinItemsOneInlineArrayNullableOneOf
minItemsOneInlineArrayNullableOneOfSchema =
  FC.coerceSchema (FC.nonEmpty MinItemsOneInlineArrayNullableOneOfItem.minItemsOneInlineArrayNullableOneOfItemSchema)