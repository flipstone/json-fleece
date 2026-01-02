{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ArrayWithNullableStrings
  ( ArrayWithNullableStrings(..)
  , arrayWithNullableStringsSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import qualified TestCases.Types.NullableString as NullableString

newtype ArrayWithNullableStrings = ArrayWithNullableStrings [Either FC.Null NullableString.NullableString]
  deriving (Show, Eq)

arrayWithNullableStringsSchema :: FC.Fleece t => FC.Schema t ArrayWithNullableStrings
arrayWithNullableStringsSchema =
  FC.coerceSchema (FC.list (FC.nullable NullableString.nullableStringSchema))