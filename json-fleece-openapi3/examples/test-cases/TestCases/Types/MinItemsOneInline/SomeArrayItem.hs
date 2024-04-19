{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MinItemsOneInline.SomeArrayItem
  ( SomeArrayItem(..)
  , someArrayItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SomeArrayItem = SomeArrayItem Bool
  deriving (Show, Eq)

someArrayItemSchema :: FC.Fleece schema => schema SomeArrayItem
someArrayItemSchema =
  FC.coerceSchema FC.boolean