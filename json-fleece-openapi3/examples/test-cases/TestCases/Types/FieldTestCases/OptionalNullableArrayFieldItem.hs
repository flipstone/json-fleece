{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.OptionalNullableArrayFieldItem
  ( OptionalNullableArrayFieldItem(..)
  , optionalNullableArrayFieldItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalNullableArrayFieldItem = OptionalNullableArrayFieldItem T.Text
  deriving (Show, Eq)

optionalNullableArrayFieldItemSchema :: FC.Fleece t => FC.Schema t OptionalNullableArrayFieldItem
optionalNullableArrayFieldItemSchema =
  FC.coerceSchema FC.text