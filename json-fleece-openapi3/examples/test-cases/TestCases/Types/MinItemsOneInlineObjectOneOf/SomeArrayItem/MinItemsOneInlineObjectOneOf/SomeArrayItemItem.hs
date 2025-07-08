{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInlineObjectOneOf.SomeArrayItem.MinItemsOneInlineObjectOneOf.SomeArrayItemItem
  ( SomeArrayItemItem(..)
  , someArrayItemItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SomeArrayItemItem = SomeArrayItemItem Bool
  deriving (Show, Eq)

someArrayItemItemSchema :: FC.Fleece schema => schema SomeArrayItemItem
someArrayItemItemSchema =
  FC.coerceSchema FC.boolean