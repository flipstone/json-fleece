{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasWithSiblings
  ( SchemaRefAliasWithSiblings
  , schemaRefAliasWithSiblingsSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.TopLevelOneOf as TopLevelOneOf

type SchemaRefAliasWithSiblings = TopLevelOneOf.TopLevelOneOf

schemaRefAliasWithSiblingsSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasWithSiblings
schemaRefAliasWithSiblingsSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SchemaRefAliasWithSiblings" "SchemaRefAliasWithSiblings")
    TopLevelOneOf.topLevelOneOfSchema