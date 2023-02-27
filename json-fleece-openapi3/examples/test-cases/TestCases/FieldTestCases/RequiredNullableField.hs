{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.RequiredNullableField
  ( RequiredNullableField(..)
  , requiredNullableFieldSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype RequiredNullableField = RequiredNullableField Text
  deriving (Show, Eq)

requiredNullableFieldSchema :: FC.Fleece schema => schema RequiredNullableField
requiredNullableFieldSchema =
  FC.coerceSchema FC.text