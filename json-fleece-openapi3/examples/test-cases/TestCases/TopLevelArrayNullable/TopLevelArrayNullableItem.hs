{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.TopLevelArrayNullable.TopLevelArrayNullableItem
  ( TopLevelArrayNullableItem(..)
  , topLevelArrayNullableItemSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TopLevelArrayNullableItem = TopLevelArrayNullableItem Text
  deriving (Show, Eq)

topLevelArrayNullableItemSchema :: FC.Fleece schema => schema TopLevelArrayNullableItem
topLevelArrayNullableItemSchema =
  FC.coerceSchema FC.text