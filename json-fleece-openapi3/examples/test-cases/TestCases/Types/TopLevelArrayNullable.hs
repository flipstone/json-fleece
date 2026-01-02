{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.TopLevelArrayNullable
  ( TopLevelArrayNullable(..)
  , topLevelArrayNullableSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import qualified TestCases.Types.TopLevelArrayNullable.TopLevelArrayNullableItem as TopLevelArrayNullableItem

newtype TopLevelArrayNullable = TopLevelArrayNullable [Either FC.Null TopLevelArrayNullableItem.TopLevelArrayNullableItem]
  deriving (Show, Eq)

topLevelArrayNullableSchema :: FC.Fleece t => FC.Schema t TopLevelArrayNullable
topLevelArrayNullableSchema =
  FC.coerceSchema (FC.list (FC.nullable TopLevelArrayNullableItem.topLevelArrayNullableItemSchema))