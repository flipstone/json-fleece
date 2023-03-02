{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.RequiredNullableField
  ( RequiredNullableField(..)
  , requiredNullableFieldSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype RequiredNullableField = RequiredNullableField T.Text
  deriving (Show, Eq)

requiredNullableFieldSchema :: FC.Fleece schema => schema RequiredNullableField
requiredNullableFieldSchema =
  FC.coerceSchema FC.text