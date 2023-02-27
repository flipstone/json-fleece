{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.TopLevelArrayNullable
  ( TopLevelArrayNullable(..)
  , topLevelArrayNullableSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import TestCases.TopLevelArrayNullable.TopLevelArrayNullableItem (TopLevelArrayNullableItem, topLevelArrayNullableItemSchema)

newtype TopLevelArrayNullable = TopLevelArrayNullable [Either FC.Null TopLevelArrayNullableItem]
  deriving (Show, Eq)

topLevelArrayNullableSchema :: FC.Fleece schema => schema TopLevelArrayNullable
topLevelArrayNullableSchema =
  FC.coerceSchema (FC.list (FC.nullable topLevelArrayNullableItemSchema))