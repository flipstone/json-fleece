{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.OptionalNullableField
  ( OptionalNullableField(..)
  , optionalNullableFieldSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalNullableField = OptionalNullableField T.Text
  deriving (Show, Eq)

optionalNullableFieldSchema :: FC.Fleece t => FC.Schema t OptionalNullableField
optionalNullableFieldSchema =
  FC.coerceSchema FC.text