{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineArrayOneOf
  ( MinItemsOneInlineArrayOneOf(..)
  , minItemsOneInlineArrayOneOfSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.MinItemsOneInlineArrayOneOf.MinItemsOneInlineArrayOneOfItem as MinItemsOneInlineArrayOneOfItem

newtype MinItemsOneInlineArrayOneOf = MinItemsOneInlineArrayOneOf (NEL.NonEmpty MinItemsOneInlineArrayOneOfItem.MinItemsOneInlineArrayOneOfItem)
  deriving (Show, Eq)

minItemsOneInlineArrayOneOfSchema :: FC.Fleece t => FC.Schema t MinItemsOneInlineArrayOneOf
minItemsOneInlineArrayOneOfSchema =
  FC.coerceSchema (FC.nonEmpty MinItemsOneInlineArrayOneOfItem.minItemsOneInlineArrayOneOfItemSchema)