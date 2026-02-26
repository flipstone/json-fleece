{-# LANGUAGE OverloadedStrings #-}

module Fleece.Markdown.Render
  ( schemaDocumentationToMarkdown
  ) where

import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyText as NET
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

import qualified Fleece.Core as FC
import Fleece.Markdown.SchemaDocumentation
  ( FieldDocumentation
      ( fieldAllowsNull
      , fieldKeyRequired
      , fieldName
      , fieldSchemaDocs
      )
  , MainEntry (ArrayEntry, EnumValues, Fields, NameOnly, NullableEntry, TaggedUnionEntry, UnionEntry, WithFormat)
  , SchemaDocumentation
    ( schemaDescription
    , schemaExcludeFromRender
    , schemaMainEntry
    , schemaName
    , schemaNullability
    )
  , SchemaNullability (NotNull, Nullable)
  , TaggedUnionMemberDocumentation (tagFields, tagValue)
  , schemaReferencesWithDescendants
  )

schemaDocumentationToMarkdown :: SchemaDocumentation -> LT.Text
schemaDocumentationToMarkdown rootDocs =
  let
    allDocs =
      schemaReferencesWithDescendants rootDocs

    nameContext =
      mkNameContext (Map.keysSet allDocs)

    docsWithoutRootInOrder =
      List.sortBy (schemaDocumentationOrder nameContext)
        . filter (not . schemaExcludeFromRender)
        . Map.elems
        . Map.delete (schemaName rootDocs)
        $ allDocs

    docSections =
      map
        (schemaMainEntryDocs nameContext)
        (rootDocs : docsWithoutRootInOrder)
  in
    LTB.toLazyText (mconcat (List.intersperse newline docSections))

newtype NameContext = NameContext
  { namesRequiringQualification :: Set.Set FC.Name
  }

mkNameContext :: Set.Set FC.Name -> NameContext
mkNameContext names =
  let
    duplicateNames =
      Set.unions
        . Map.elems
        . Map.filter (\list -> length list > 1)
        . Map.fromListWith (<>)
        . fmap (\name -> (FC.nameUnqualified name, Set.singleton name))
        . Set.toList
        $ names
  in
    NameContext
      { namesRequiringQualification = duplicateNames
      }

schemaDocumentationOrder ::
  NameContext ->
  SchemaDocumentation ->
  SchemaDocumentation ->
  Ordering
schemaDocumentationOrder context =
  compare `on` (nameToText context . schemaName)

renderName :: NameContext -> FC.Name -> LTB.Builder
renderName context =
  markdownText . nameToText context

nameToText :: NameContext -> FC.Name -> T.Text
nameToText context name =
  T.pack $
    if Set.member name (namesRequiringQualification context)
      then FC.nameToString name
      else FC.nameUnqualified name

renderDescription :: Maybe NET.NonEmptyText -> LTB.Builder
renderDescription mbDesc =
  case mbDesc of
    Just desc -> quote (markdownText (NET.toText desc)) <> newline <> newline
    Nothing -> mempty

schemaFieldTypeDocs :: NameContext -> SchemaDocumentation -> LTB.Builder
schemaFieldTypeDocs nameContext schemaDocs =
  let
    name =
      renderName nameContext . schemaName $
        case schemaNullability schemaDocs of
          Nullable notNullSchemaDocs -> notNullSchemaDocs
          NotNull -> schemaDocs
  in
    case schemaMainEntry schemaDocs of
      WithFormat format _entry
        | schemaExcludeFromRender schemaDocs ->
            -- If this schema has a format that has been declared and also is going
            -- to be excluded from the render (so it's main entry will never be seen)
            -- then include the format in the field type docs.
            name <> " (format: " <> LTB.fromString format <> ")"
      _ ->
        name

schemaMainEntryDocs :: NameContext -> SchemaDocumentation -> LTB.Builder
schemaMainEntryDocs nameContext schemaDocs =
  h1 (renderName nameContext (schemaName schemaDocs))
    <> newline
    <> newline
    <> renderDescription (schemaDescription schemaDocs)
    <> mainEntryDocs nameContext (schemaMainEntry schemaDocs)

mainEntryDocs :: NameContext -> MainEntry -> LTB.Builder
mainEntryDocs nameContext entry =
  case entry of
    NameOnly name ->
      renderName nameContext name
    Fields fields ->
      fieldsHeader <> foldMap (fieldRow nameContext) fields
    EnumValues enumValues ->
      markdownText "Enum values:"
        <> newline
        <> newline
        <> foldMap (listItem . markdownText) enumValues
    ArrayEntry itemEntry ->
      markdownText "Array of:"
        <> newline
        <> newline
        <> mainEntryDocs nameContext itemEntry
    NullableEntry itemEntry ->
      mainEntryDocs nameContext itemEntry
        <> newline
        <> markdownText "(This value may be null)"
        <> newline
    UnionEntry memberNames ->
      "Any one of the following"
        <> newline
        <> newline
        <> foldMap (listItem . renderName nameContext) memberNames
    TaggedUnionEntry tagProperty memberDocs ->
      "One of the following field structures, depending on the value found in the "
        <> markdownText tagProperty
        <> " field."
        <> newline
        <> newline
        <> foldMap (taggedUnionMember nameContext tagProperty) memberDocs
    WithFormat formatString itemEntry ->
      mainEntryDocs nameContext itemEntry
        <> newline
        <> newline
        <> "format: "
        <> markdownText (T.pack formatString)
        <> newline

fieldsHeader :: LTB.Builder
fieldsHeader =
  markdownText "|Field|Key Required|Null Allowed|Type|"
    <> newline
    <> markdownText "|---|---|---|---|"
    <> newline

fieldRow :: NameContext -> FieldDocumentation -> LTB.Builder
fieldRow nameContext fieldDocs =
  pipe
    <> markdownText (fieldName fieldDocs)
    <> pipe
    <> yesOrNo (fieldKeyRequired fieldDocs)
    <> pipe
    <> yesOrNo (fieldAllowsNull fieldDocs)
    <> pipe
    <> schemaFieldTypeDocs nameContext (fieldSchemaDocs fieldDocs)
    <> pipe
    <> newline

taggedUnionMember :: NameContext -> T.Text -> TaggedUnionMemberDocumentation -> LTB.Builder
taggedUnionMember nameContext tagProperty memberDocs =
  let
    headerText =
      markdownText (tagProperty)
        <> " = "
        <> markdownText (tagValue memberDocs)
  in
    h2 headerText
      <> newline
      <> newline
      <> "When the \""
      <> markdownText tagProperty
      <> "\" field has the value \""
      <> markdownText (tagValue memberDocs)
      <> "\", the object has the following fields:"
      <> newline
      <> newline
      <> fieldsHeader
      <> foldMap (fieldRow nameContext) (tagFields memberDocs)
      <> newline

pipe :: LTB.Builder
pipe =
  markdownText "|"

newline :: LTB.Builder
newline =
  markdownText "\n"

yes :: LTB.Builder
yes =
  markdownText "yes"

no :: LTB.Builder
no =
  markdownText "no"

yesOrNo :: Bool -> LTB.Builder
yesOrNo b =
  case b of
    True -> yes
    False -> no

listItem :: LTB.Builder -> LTB.Builder
listItem item =
  markdownText "- " <> item <> newline

markdownText :: T.Text -> LTB.Builder
markdownText =
  LTB.fromText . T.replace "_" "\\_"

h1 :: LTB.Builder -> LTB.Builder
h1 builder =
  markdownText "# " <> builder

h2 :: LTB.Builder -> LTB.Builder
h2 builder =
  markdownText "## " <> builder

quote :: LTB.Builder -> LTB.Builder
quote builder =
  markdownText "> " <> builder
