{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SchemaRefAlias
  ( SchemaRefAlias(..)
  , schemaRefAliasSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.AStringType as AStringType

newtype SchemaRefAlias = SchemaRefAlias AStringType.AStringType
  deriving (Show, Eq)

schemaRefAliasSchema :: FC.Fleece t => FC.Schema t SchemaRefAlias
schemaRefAliasSchema =
  FC.coerceSchema AStringType.aStringTypeSchema