module Fleece.Markdown.SchemaDocumentation
  ( SchemaDocumentation
      ( SchemaDocumentation
      , schemaName
      , schemaExcludeFromRender
      , schemaNullability
      , schemaMainEntry
      , schemaReferences
      )
  , schemaSelfReference
  , schemaReferencesWithDescendants
  , SchemaNullability (NotNull, Nullable)
  , MainEntry (NameOnly, Fields, EnumValues, ArrayEntry, NullableEntry, UnionEntry, TaggedUnionEntry)
  , FieldDocumentation
    ( FieldDocumentation
    , fieldName
    , fieldKeyRequired
    , fieldAllowsNull
    , fieldSchemaDocs
    )
  , FieldList
  , TaggedUnionMemberDocumentation
    ( TaggedUnionMemberDocumentation
    , tagValue
    , tagFields
    )
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

schemaSelfReference :: SchemaDocumentation -> Map.Map FC.Name SchemaDocumentation
schemaSelfReference schemaDoc =
  Map.singleton (schemaName schemaDoc) schemaDoc

schemaReferencesWithDescendants ::
  SchemaDocumentation ->
  Map.Map FC.Name SchemaDocumentation
schemaReferencesWithDescendants =
  let
    go results schemaDocs =
      if Map.member (schemaName schemaDocs) results
        then results
        else
          let
            resultsWithSelf =
              Map.insert (schemaName schemaDocs) schemaDocs results
          in
            foldr
              (flip go)
              resultsWithSelf
              (schemaReferences schemaDocs)
  in
    go Map.empty

data SchemaNullability
  = NotNull
  | Nullable SchemaDocumentation

data MainEntry
  = NameOnly FC.Name
  | Fields FieldList
  | EnumValues [T.Text]
  | ArrayEntry MainEntry
  | NullableEntry MainEntry
  | UnionEntry [FC.Name]
  | TaggedUnionEntry T.Text [TaggedUnionMemberDocumentation]

data FieldDocumentation = FieldDocumentation
  { fieldName :: T.Text
  , fieldKeyRequired :: Bool
  , fieldAllowsNull :: Bool
  , fieldSchemaDocs :: SchemaDocumentation
  }

type FieldList = DList.DList FieldDocumentation

data TaggedUnionMemberDocumentation = TaggedUnionMemberDocumentation
  { tagValue :: T.Text
  , tagFields :: FieldList
  }
