{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.RequiredNullableField
  ( RequiredNullableField(..)
  , requiredNullableFieldSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype RequiredNullableField = RequiredNullableField T.Text
  deriving (Show, Eq)

requiredNullableFieldSchema :: FC.Fleece t => FC.Schema t RequiredNullableField
requiredNullableFieldSchema =
  FC.coerceSchema FC.text