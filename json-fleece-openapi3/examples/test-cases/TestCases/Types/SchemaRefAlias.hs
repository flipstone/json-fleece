{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAlias
  ( SchemaRefAlias
  , schemaRefAliasSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.AStringType as AStringType

type SchemaRefAlias = AStringType.AStringType

schemaRefAliasSchema :: FC.Fleece t => FC.Schema t SchemaRefAlias
schemaRefAliasSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SchemaRefAlias" "SchemaRefAlias")
    AStringType.aStringTypeSchema