{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasOfAlias
  ( SchemaRefAliasOfAlias(..)
  , schemaRefAliasOfAliasSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.SchemaRefAlias as SchemaRefAlias

newtype SchemaRefAliasOfAlias = SchemaRefAliasOfAlias SchemaRefAlias.SchemaRefAlias
  deriving (Show, Eq)

schemaRefAliasOfAliasSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasOfAlias
schemaRefAliasOfAliasSchema =
  FC.coerceSchema SchemaRefAlias.schemaRefAliasSchema