{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.OptionalNullableField
  ( OptionalNullableField(..)
  , optionalNullableFieldSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OptionalNullableField = OptionalNullableField Text
  deriving (Show, Eq)

optionalNullableFieldSchema :: FC.Fleece schema => schema OptionalNullableField
optionalNullableFieldSchema =
  FC.coerceSchema FC.text