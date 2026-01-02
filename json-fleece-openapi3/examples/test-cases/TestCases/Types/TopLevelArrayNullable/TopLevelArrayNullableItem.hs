{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.TopLevelArrayNullable.TopLevelArrayNullableItem
  ( TopLevelArrayNullableItem(..)
  , topLevelArrayNullableItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TopLevelArrayNullableItem = TopLevelArrayNullableItem T.Text
  deriving (Show, Eq)

topLevelArrayNullableItemSchema :: FC.Fleece t => FC.Schema t TopLevelArrayNullableItem
topLevelArrayNullableItemSchema =
  FC.coerceSchema FC.text