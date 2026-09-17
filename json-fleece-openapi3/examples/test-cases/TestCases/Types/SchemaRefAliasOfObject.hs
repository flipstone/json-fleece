{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasOfObject
  ( SchemaRefAliasOfObject
  , schemaRefAliasOfObjectSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.ReferenceOneOf as ReferenceOneOf

type SchemaRefAliasOfObject = ReferenceOneOf.ReferenceOneOf

schemaRefAliasOfObjectSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasOfObject
schemaRefAliasOfObjectSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SchemaRefAliasOfObject" "SchemaRefAliasOfObject")
    ReferenceOneOf.referenceOneOfSchema