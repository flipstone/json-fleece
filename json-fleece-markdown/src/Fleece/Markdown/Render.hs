{-# LANGUAGE OverloadedStrings #-}

module Fleece.Markdown.Render
  ( schemaDocumentationToMarkdown
  ) where

import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
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
  , MainEntry (ArrayEntry, EnumValues, Fields, NameOnly, NullableEntry, UnionEntry)
  , SchemaDocumentation
    ( schemaExcludeFromRender
    , schemaMainEntry
    , schemaName
    , schemaNullability
    )
  , SchemaNullability (NotNull, Nullable)
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

schemaFieldTypeDocs :: NameContext -> SchemaDocumentation -> LTB.Builder
schemaFieldTypeDocs nameContext schemaDocs =
  renderName nameContext . schemaName $
    case schemaNullability schemaDocs of
      Nullable notNullSchemaDocs -> notNullSchemaDocs
      NotNull -> schemaDocs

schemaMainEntryDocs :: NameContext -> SchemaDocumentation -> LTB.Builder
schemaMainEntryDocs nameContext schemaDocs =
  h1 (renderName nameContext (schemaName schemaDocs))
    <> newline
    <> newline
    <> mainEntryDocs nameContext (schemaMainEntry schemaDocs)

mainEntryDocs :: NameContext -> MainEntry -> LTB.Builder
mainEntryDocs nameContext entry =
  case entry of
    NameOnly name ->
      renderName nameContext $ name
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

h1 :: LTB.Builder -> LTB.Builder
h1 builder =
  markdownText "# " <> builder

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
