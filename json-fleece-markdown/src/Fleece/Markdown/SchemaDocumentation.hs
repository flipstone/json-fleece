module Fleece.Markdown.SchemaDocumentation
  ( SchemaDocumentation
      ( SchemaDocumentation
      , schemaName
      , schemaExcludeFromRender
      , schemaNullability
      , schemaMainEntry
      , schemaReferences
      )
  , schemaReferencesIncludingSelf
  , SchemaNullability (NotNull, Nullable)
  , MainEntry (NameOnly, Fields, EnumValues, ArrayEntry, NullableEntry)
  , FieldDocumentation
    ( FieldDocumentation
    , fieldName
    , fieldKeyRequired
    , fieldAllowsNull
    , fieldSchemaDocs
    )
  , FieldList
  ) where

import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified Fleece.Core as FC

data SchemaDocumentation = SchemaDocumentation
  { schemaName :: FC.Name
  , schemaExcludeFromRender :: Bool
  , schemaNullability :: SchemaNullability
  , schemaMainEntry :: MainEntry
  , schemaReferences :: Map.Map FC.Name SchemaDocumentation
  }

schemaReferencesIncludingSelf ::
  SchemaDocumentation ->
  Map.Map FC.Name SchemaDocumentation
schemaReferencesIncludingSelf schemaDocs =
  Map.singleton (schemaName schemaDocs) schemaDocs
    <> schemaReferences schemaDocs

data SchemaNullability
  = NotNull
  | Nullable SchemaDocumentation

data MainEntry
  = NameOnly FC.Name
  | Fields FieldList
  | EnumValues [T.Text]
  | ArrayEntry MainEntry
  | NullableEntry MainEntry

data FieldDocumentation = FieldDocumentation
  { fieldName :: T.Text
  , fieldKeyRequired :: Bool
  , fieldAllowsNull :: Bool
  , fieldSchemaDocs :: SchemaDocumentation
  }

type FieldList = DList.DList FieldDocumentation
