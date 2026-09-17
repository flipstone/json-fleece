{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ObjectWithSchemaRefAliasField
  ( ObjectWithSchemaRefAliasField(..)
  , objectWithSchemaRefAliasFieldSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.SchemaRefAlias as SchemaRefAlias
import qualified TestCases.Types.SchemaRefAliasOfAlias as SchemaRefAliasOfAlias
import qualified TestCases.Types.SchemaRefAliasWithSiblings as SchemaRefAliasWithSiblings

data ObjectWithSchemaRefAliasField = ObjectWithSchemaRefAliasField
  { aliased :: Maybe SchemaRefAlias.SchemaRefAlias
  , aliasedTwice :: Maybe SchemaRefAliasOfAlias.SchemaRefAliasOfAlias
  , aliasedWithSiblings :: Maybe SchemaRefAliasWithSiblings.SchemaRefAliasWithSiblings -- ^ Keys alongside a $ref are ignored, but the description is kept
  }
  deriving (Eq, Show)

objectWithSchemaRefAliasFieldSchema :: FC.Fleece t => FC.Schema t ObjectWithSchemaRefAliasField
objectWithSchemaRefAliasFieldSchema =
  FC.object $
    FC.constructor ObjectWithSchemaRefAliasField
      #+ FC.optional "aliased" aliased SchemaRefAlias.schemaRefAliasSchema
      #+ FC.optional "aliasedTwice" aliasedTwice SchemaRefAliasOfAlias.schemaRefAliasOfAliasSchema
      #+ FC.optional "aliasedWithSiblings" aliasedWithSiblings SchemaRefAliasWithSiblings.schemaRefAliasWithSiblingsSchema