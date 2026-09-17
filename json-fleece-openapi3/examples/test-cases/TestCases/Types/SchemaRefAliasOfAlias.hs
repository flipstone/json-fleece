{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasOfAlias
  ( SchemaRefAliasOfAlias
  , schemaRefAliasOfAliasSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.SchemaRefAlias as SchemaRefAlias

type SchemaRefAliasOfAlias = SchemaRefAlias.SchemaRefAlias

schemaRefAliasOfAliasSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasOfAlias
schemaRefAliasOfAliasSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SchemaRefAliasOfAlias" "SchemaRefAliasOfAlias")
    SchemaRefAlias.schemaRefAliasSchema