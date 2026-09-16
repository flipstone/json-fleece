{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAliasOfObject
  ( SchemaRefAliasOfObject(..)
  , schemaRefAliasOfObjectSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.ReferenceOneOf as ReferenceOneOf

newtype SchemaRefAliasOfObject = SchemaRefAliasOfObject ReferenceOneOf.ReferenceOneOf
  deriving (Show, Eq)

schemaRefAliasOfObjectSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasOfObject
schemaRefAliasOfObjectSchema =
  FC.coerceSchema ReferenceOneOf.referenceOneOfSchema