{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.NullableArrayFieldItem
  ( NullableArrayFieldItem(..)
  , nullableArrayFieldItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NullableArrayFieldItem = NullableArrayFieldItem T.Text
  deriving (Show, Eq)

nullableArrayFieldItemSchema :: FC.Fleece schema => schema NullableArrayFieldItem
nullableArrayFieldItemSchema =
  FC.coerceSchema FC.text