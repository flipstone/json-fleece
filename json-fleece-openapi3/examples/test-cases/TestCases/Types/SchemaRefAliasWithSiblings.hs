{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasWithSiblings
  ( SchemaRefAliasWithSiblings(..)
  , schemaRefAliasWithSiblingsSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.TopLevelOneOf as TopLevelOneOf

newtype SchemaRefAliasWithSiblings = SchemaRefAliasWithSiblings TopLevelOneOf.TopLevelOneOf
  deriving (Show, Eq)

schemaRefAliasWithSiblingsSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasWithSiblings
schemaRefAliasWithSiblingsSchema =
  FC.coerceSchema TopLevelOneOf.topLevelOneOfSchema