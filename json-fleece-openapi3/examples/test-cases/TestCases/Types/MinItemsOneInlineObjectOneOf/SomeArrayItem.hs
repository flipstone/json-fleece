{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineObjectOneOf.SomeArrayItem
  ( SomeArrayItem(..)
  , someArrayItemSchema
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.MinItemsOneInlineObjectOneOf.SomeArrayItem.MinItemsOneInlineObjectOneOf.SomeArrayItemItem as SomeArrayItemItem

newtype SomeArrayItem = SomeArrayItem (NEL.NonEmpty SomeArrayItemItem.SomeArrayItemItem)
  deriving (Show, Eq)

someArrayItemSchema :: FC.Fleece t => FC.Schema t SomeArrayItem
someArrayItemSchema =
  FC.coerceSchema (FC.nonEmpty SomeArrayItemItem.someArrayItemItemSchema)