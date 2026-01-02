{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.NullableArrayFieldItem
  ( NullableArrayFieldItem(..)
  , nullableArrayFieldItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NullableArrayFieldItem = NullableArrayFieldItem T.Text
  deriving (Show, Eq)

nullableArrayFieldItemSchema :: FC.Fleece t => FC.Schema t NullableArrayFieldItem
nullableArrayFieldItemSchema =
  FC.coerceSchema FC.text