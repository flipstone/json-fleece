{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.TopLevelArrayNullable
  ( TopLevelArrayNullable(..)
  , topLevelArrayNullableSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import qualified TestCases.TopLevelArrayNullable.TopLevelArrayNullableItem as TopLevelArrayNullableItem

newtype TopLevelArrayNullable = TopLevelArrayNullable [Either FC.Null TopLevelArrayNullableItem.TopLevelArrayNullableItem]
  deriving (Show, Eq)

topLevelArrayNullableSchema :: FC.Fleece schema => schema TopLevelArrayNullable
topLevelArrayNullableSchema =
  FC.coerceSchema (FC.list (FC.nullable TopLevelArrayNullableItem.topLevelArrayNullableItemSchema))