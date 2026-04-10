{- | Data types for representing schema documentation, used by the Markdown
Fleece instance to generate documentation from Fleece schemas.
-}
module Fleece.Markdown.SchemaDocumentation
  ( SchemaDocumentation
      ( SchemaDocumentation
      , schemaName
      , schemaDescription
      , schemaExcludeFromRender
      , schemaNullability
      , schemaAnnotations
      , schemaMainEntry
      , schemaReferences
      )
  , schemaSelfReference
  , schemaReferencesWithDescendants
  , SchemaNullability (NotNull, Nullable)
  , SchemaAnnotations
    ( SchemaAnnotations
    , annotationFormat
    , annotationMinLength
    , annotationMaxLength
    , annotationMinItems
    , annotationMaxItems
    , annotationMinimum
    , annotationMaximum
    )
  , emptyAnnotations
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
import qualified Data.NonEmptyText as NET
import qualified Data.Text as T

import qualified Fleece.Core as FC

{- | Documentation for a single JSON schema, including its name, description,
field structure, and references.
-}
data SchemaDocumentation = SchemaDocumentation
  { schemaName :: FC.Name
  -- ^ The name of the documented schema.
  , schemaDescription :: Maybe NET.NonEmptyText
  -- ^ An optional human-readable description of the schema.
  , schemaExcludeFromRender :: Bool
  -- ^ Whether this schema should be excluded from top-level rendering (e.g., primitive types).
  , schemaNullability :: SchemaNullability
  -- ^ Whether this schema allows null values.
  , schemaAnnotations :: SchemaAnnotations
  -- ^ Format and constraint annotations for the schema.
  , schemaMainEntry :: MainEntry
  -- ^ The main documentation entry describing the schema's structure.
  , schemaReferences :: Map.Map FC.Name SchemaDocumentation
  -- ^ Schemas referenced by this schema, keyed by name.
  }

-- | Creates a singleton reference map containing just this schema, keyed by its name.
schemaSelfReference :: SchemaDocumentation -> Map.Map FC.Name SchemaDocumentation
schemaSelfReference schemaDoc =
  Map.singleton (schemaName schemaDoc) schemaDoc

-- | Recursively collects all schemas referenced by a given schema, including transitive references.
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

-- | Indicates whether a schema allows null values.
data SchemaNullability
  = -- | The schema does not allow null values.
    NotNull
  | -- | The schema allows null values; contains the inner non-null schema documentation.
    Nullable SchemaDocumentation

-- | The main structural entry for a schema's documentation.
data MainEntry
  = -- | A simple named reference with no further structure.
    NameOnly FC.Name
  | -- | An object schema with a list of field documentation entries.
    Fields FieldList
  | -- | An enum schema with a list of possible text values.
    EnumValues [T.Text]
  | -- | An array schema wrapping an inner entry.
    ArrayEntry MainEntry
  | -- | A nullable schema wrapping an inner entry.
    NullableEntry MainEntry
  | -- | An anonymous union schema with a list of member names.
    UnionEntry [FC.Name]
  | -- | A tagged union with a discriminator property name and member documentation.
    TaggedUnionEntry T.Text [TaggedUnionMemberDocumentation]

-- | Constraint and format annotations from the schema definition.
data SchemaAnnotations = SchemaAnnotations
  { annotationFormat :: Maybe String
  -- ^ An optional format string (e.g., @"date-time"@, @"int32"@).
  , annotationMinLength :: Maybe Integer
  -- ^ An optional minimum text length constraint.
  , annotationMaxLength :: Maybe Integer
  -- ^ An optional maximum text length constraint.
  , annotationMinItems :: Maybe Integer
  -- ^ An optional minimum array item count constraint.
  , annotationMaxItems :: Maybe Integer
  -- ^ An optional maximum array item count constraint.
  , annotationMinimum :: Maybe Integer
  -- ^ An optional minimum numeric value constraint.
  , annotationMaximum :: Maybe Integer
  -- ^ An optional maximum numeric value constraint.
  }

-- | An 'SchemaAnnotations' value with all fields set to 'Nothing'.
emptyAnnotations :: SchemaAnnotations
emptyAnnotations =
  SchemaAnnotations
    { annotationFormat = Nothing
    , annotationMinLength = Nothing
    , annotationMaxLength = Nothing
    , annotationMinItems = Nothing
    , annotationMaxItems = Nothing
    , annotationMinimum = Nothing
    , annotationMaximum = Nothing
    }

-- | Documentation for a single field within a JSON object.
data FieldDocumentation = FieldDocumentation
  { fieldName :: T.Text
  -- ^ The JSON key name of the field.
  , fieldKeyRequired :: Bool
  -- ^ Whether the field key must be present in the JSON object.
  , fieldAllowsNull :: Bool
  -- ^ Whether the field value may be null.
  , fieldSchemaDocs :: SchemaDocumentation
  -- ^ The schema documentation for the field's value type.
  }

-- | A difference list of 'FieldDocumentation' entries.
type FieldList = DList.DList FieldDocumentation

-- | Documentation for a single member of a tagged union.
data TaggedUnionMemberDocumentation = TaggedUnionMemberDocumentation
  { tagValue :: T.Text
  -- ^ The tag value that identifies this union member.
  , tagFields :: FieldList
  -- ^ The field documentation for the object structure when this tag value is present.
  }
