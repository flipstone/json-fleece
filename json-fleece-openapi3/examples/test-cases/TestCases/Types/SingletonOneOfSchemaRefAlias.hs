{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonOneOfSchemaRefAlias
  ( SingletonOneOfSchemaRefAlias
  , singletonOneOfSchemaRefAliasSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.SchemaRefAlias as SchemaRefAlias

type SingletonOneOfSchemaRefAlias = SchemaRefAlias.SchemaRefAlias

singletonOneOfSchemaRefAliasSchema :: FC.Fleece t => FC.Schema t SingletonOneOfSchemaRefAlias
singletonOneOfSchemaRefAliasSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SingletonOneOfSchemaRefAlias" "SingletonOneOfSchemaRefAlias")
    SchemaRefAlias.schemaRefAliasSchema