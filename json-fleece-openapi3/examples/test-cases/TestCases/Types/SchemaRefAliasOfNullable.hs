{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasOfNullable
  ( SchemaRefAliasOfNullable
  , schemaRefAliasOfNullableSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Either)
import qualified TestCases.Types.NullableString as NullableString

type SchemaRefAliasOfNullable = Either FC.Null NullableString.NullableString

schemaRefAliasOfNullableSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasOfNullable
schemaRefAliasOfNullableSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SchemaRefAliasOfNullable" "SchemaRefAliasOfNullable")
    (FC.nullable NullableString.nullableStringSchema)