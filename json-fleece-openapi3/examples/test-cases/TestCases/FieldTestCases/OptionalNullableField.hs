{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.OptionalNullableField
  ( OptionalNullableField(..)
  , optionalNullableFieldSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalNullableField = OptionalNullableField T.Text
  deriving (Show, Eq)

optionalNullableFieldSchema :: FC.Fleece schema => schema OptionalNullableField
optionalNullableFieldSchema =
  FC.coerceSchema FC.text