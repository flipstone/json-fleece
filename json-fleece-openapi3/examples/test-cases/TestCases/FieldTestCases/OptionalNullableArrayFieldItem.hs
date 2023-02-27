{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.OptionalNullableArrayFieldItem
  ( OptionalNullableArrayFieldItem(..)
  , optionalNullableArrayFieldItemSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalNullableArrayFieldItem = OptionalNullableArrayFieldItem Text
  deriving (Show, Eq)

optionalNullableArrayFieldItemSchema :: FC.Fleece schema => schema OptionalNullableArrayFieldItem
optionalNullableArrayFieldItemSchema =
  FC.coerceSchema FC.text