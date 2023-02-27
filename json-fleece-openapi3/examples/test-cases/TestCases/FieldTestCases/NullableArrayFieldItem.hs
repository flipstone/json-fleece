{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.NullableArrayFieldItem
  ( NullableArrayFieldItem(..)
  , nullableArrayFieldItemSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NullableArrayFieldItem = NullableArrayFieldItem Text
  deriving (Show, Eq)

nullableArrayFieldItemSchema :: FC.Fleece schema => schema NullableArrayFieldItem
nullableArrayFieldItemSchema =
  FC.coerceSchema FC.text