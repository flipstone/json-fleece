{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasOfNullable
  ( SchemaRefAliasOfNullable(..)
  , schemaRefAliasOfNullableSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import qualified TestCases.Types.NullableString as NullableString

newtype SchemaRefAliasOfNullable = SchemaRefAliasOfNullable (Either FC.Null NullableString.NullableString)
  deriving (Show, Eq)

schemaRefAliasOfNullableSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasOfNullable
schemaRefAliasOfNullableSchema =
  FC.coerceSchema (FC.nullable NullableString.nullableStringSchema)