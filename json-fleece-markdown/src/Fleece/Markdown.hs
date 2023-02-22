{-# LANGUAGE TypeFamilies #-}

module Fleece.Markdown
  ( Markdown
  , renderMarkdown
  ) where

import Data.DList (DList, snoc)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

import qualified Fleece.Core as FC

data Markdown a = Markdown SchemaDocumentation

renderMarkdown :: Markdown a -> LT.Text
renderMarkdown (Markdown schemaDocs) =
  LTB.toLazyText $
    schemaMainEntryDocs schemaDocs
      <> foldMap
        (\docs -> newline <> schemaMainEntryDocs docs)
        ( Map.filter
            (not . schemaExcludeFromRender)
            (schemaReferences schemaDocs)
        )

data SchemaDocumentation = SchemaDocumentation
  { schemaName :: String
  , schemaExcludeFromRender :: Bool
  , schemaNullability :: SchemaNullability
  , schemaMainEntryDocs :: LTB.Builder
  , schemaFieldTypeDocs :: LTB.Builder
  , schemaReferences :: Map.Map String SchemaDocumentation
  }

schemaReferencesIncludingSelf ::
  SchemaDocumentation ->
  Map.Map String SchemaDocumentation
schemaReferencesIncludingSelf schemaDocs =
  Map.singleton (schemaName schemaDocs) schemaDocs
    <> schemaReferences schemaDocs

data FieldDocumentation = FieldDocumentation
  { fieldName :: String
  , fieldKeyRequired :: Bool
  , fieldAllowsNull :: Bool
  , fieldSchemaDocs :: SchemaDocumentation
  }

primitiveMarkdown :: String -> Markdown a
primitiveMarkdown name =
  let
    nameBuilder =
      LTB.fromString name
  in
    Markdown $
      SchemaDocumentation
        { schemaName = name
        , schemaExcludeFromRender = True
        , schemaNullability = NotNull
        , schemaMainEntryDocs = nameBuilder
        , schemaFieldTypeDocs = nameBuilder
        , schemaReferences = Map.empty
        }

markNullable :: Markdown a -> Markdown (Maybe a)
markNullable (Markdown schemaDocs) =
  Markdown $
    SchemaDocumentation
      { schemaName = "nullable " <> schemaName schemaDocs
      , schemaExcludeFromRender = schemaExcludeFromRender schemaDocs
      , schemaNullability = Nullable
      , schemaMainEntryDocs = schemaMainEntryDocs schemaDocs
      , schemaFieldTypeDocs = schemaFieldTypeDocs schemaDocs
      , schemaReferences = schemaReferences schemaDocs
      }

data SchemaNullability
  = NotNull
  | Nullable

instance FC.Fleece Markdown where
  newtype Object Markdown _object _a
    = Object (DList FieldDocumentation)

  newtype Field Markdown _object _a
    = Field FieldDocumentation

  newtype EmbeddedObject Markdown _object _a
    = EmbeddedObject (DList FieldDocumentation)

  number =
    primitiveMarkdown "number"

  text =
    primitiveMarkdown "string"

  boolean =
    primitiveMarkdown "boolean"

  array (Markdown itemSchemaDocs) =
    let
      arrayDescription =
        LTB.fromString "array of "
          <> schemaFieldTypeDocs itemSchemaDocs
          <> case schemaNullability itemSchemaDocs of
            NotNull -> mempty
            Nullable -> LTB.fromString " (nullable)"
    in
      Markdown $
        SchemaDocumentation
          { schemaName = "array of " <> schemaName itemSchemaDocs
          , schemaExcludeFromRender = True
          , schemaNullability = NotNull
          , schemaMainEntryDocs = arrayDescription
          , schemaFieldTypeDocs = arrayDescription
          , schemaReferences = schemaReferences itemSchemaDocs
          }

  null =
    primitiveMarkdown "null"

  nullable =
    markNullable

  required name _accessor fieldSchema =
    Field (mkFieldDocs name Nothing fieldSchema)

  optionalField nullBehavior name _accessor fieldSchema =
    Field (mkFieldDocs name (Just nullBehavior) fieldSchema)

  constructor _f =
    Object mempty

  field (Object fields) (Field fieldDocs) =
    Object (snoc fields fieldDocs)

  embed (Object fields) (EmbeddedObject embeddedFields) =
    Object (fields <> embeddedFields)

  embedded _accessor (Object fields) =
    EmbeddedObject fields

  objectNamed name (Object fields) =
    let
      mainEntry =
        h1 name
          <> newline
          <> newline
          <> fieldsHeader
          <> foldMap fieldRow fields

      allReferences =
        foldMap (schemaReferencesIncludingSelf . fieldSchemaDocs) fields
    in
      Markdown $
        SchemaDocumentation
          { schemaName = name
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
          , schemaMainEntryDocs = mainEntry
          , schemaFieldTypeDocs = LTB.fromString name
          , schemaReferences = allReferences
          }

  validateNamed name _check _unvalidate (Markdown schemaDocs) =
    let
      mainEntry =
        h1 name
          <> newline
          <> newline
          <> schemaMainEntryDocs schemaDocs
          <> LTB.fromString " (with validation restrictions)"
          <> newline
    in
      Markdown $
        SchemaDocumentation
          { schemaName = name
          , schemaExcludeFromRender = schemaExcludeFromRender schemaDocs
          , schemaNullability = schemaNullability schemaDocs
          , schemaMainEntryDocs = mainEntry
          , schemaFieldTypeDocs = LTB.fromString name
          , schemaReferences = schemaReferences schemaDocs
          }

  boundedEnumNamed name toText =
    let
      enumValues =
        foldMap
          (listItem . LTB.fromText . toText)
          [minBound .. maxBound]

      mainEntry =
        h1 name
          <> newline
          <> newline
          <> LTB.fromString "Enum values:"
          <> newline
          <> newline
          <> enumValues
    in
      Markdown $
        SchemaDocumentation
          { schemaName = name
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
          , schemaMainEntryDocs = mainEntry
          , schemaFieldTypeDocs = LTB.fromString name
          , schemaReferences = Map.empty
          }

h1 :: String -> LTB.Builder
h1 str =
  LTB.fromString "# " <> LTB.fromString str

fieldsHeader :: LTB.Builder
fieldsHeader =
  LTB.fromString "|Field|Key Required|Null Allowed|Type|"
    <> newline
    <> LTB.fromString "|---|---|---|---|"
    <> newline

mkFieldDocs ::
  String ->
  Maybe FC.NullBehavior ->
  Markdown a ->
  FieldDocumentation
mkFieldDocs name mbNullBehavior (Markdown schemaDocs) =
  let
    doesSchemaAllowNull =
      case schemaNullability schemaDocs of
        NotNull -> False
        Nullable -> True

    required =
      case mbNullBehavior of
        Nothing -> True
        Just _ -> False

    nullAllowed =
      case mbNullBehavior of
        Nothing -> doesSchemaAllowNull
        Just FC.EmitNull_AcceptNull -> True
        Just FC.OmitKey_AcceptNull -> True
        Just FC.OmitKey_DelegateNull -> doesSchemaAllowNull
  in
    FieldDocumentation
      { fieldName = name
      , fieldKeyRequired = required
      , fieldAllowsNull = nullAllowed
      , fieldSchemaDocs = schemaDocs
      }

fieldRow :: FieldDocumentation -> LTB.Builder
fieldRow fieldDocs =
  pipe
    <> LTB.fromString (fieldName fieldDocs)
    <> pipe
    <> yesOrNo (fieldKeyRequired fieldDocs)
    <> pipe
    <> yesOrNo (fieldAllowsNull fieldDocs)
    <> pipe
    <> schemaFieldTypeDocs (fieldSchemaDocs fieldDocs)
    <> pipe
    <> newline

pipe :: LTB.Builder
pipe =
  LTB.fromString "|"

newline :: LTB.Builder
newline =
  LTB.fromString "\n"

yes :: LTB.Builder
yes =
  LTB.fromString "yes"

no :: LTB.Builder
no =
  LTB.fromString "no"

yesOrNo :: Bool -> LTB.Builder
yesOrNo b =
  case b of
    True -> yes
    False -> no

listItem :: LTB.Builder -> LTB.Builder
listItem item =
  LTB.fromString "- " <> item <> newline
