{-# LANGUAGE TypeFamilies #-}

module Fleece.Markdown.FleeceInstance
  ( Markdown
  , renderMarkdown
  , schemaDocumentation
  ) where

import Data.Coerce (coerce)
import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Fleece.Core as FC
import Fleece.Markdown.Render (schemaDocumentationToMarkdown)
import Fleece.Markdown.SchemaDocumentation
  ( FieldDocumentation
      ( FieldDocumentation
      , fieldAllowsNull
      , fieldKeyRequired
      , fieldName
      , fieldSchemaDocs
      )
  , FieldList
  , MainEntry (ArrayEntry, EnumValues, Fields, NameOnly, NullableEntry, TaggedUnionEntry, UnionEntry)
  , SchemaAnnotations (annotationFormat, annotationMaxItems, annotationMaxLength, annotationMaximum, annotationMinItems, annotationMinLength, annotationMinimum)
  , SchemaDocumentation
    ( SchemaDocumentation
    , schemaAnnotations
    , schemaDescription
    , schemaExcludeFromRender
    , schemaMainEntry
    , schemaName
    , schemaNullability
    , schemaReferences
    )
  , SchemaNullability (NotNull, Nullable)
  , TaggedUnionMemberDocumentation (TaggedUnionMemberDocumentation, tagFields, tagValue)
  , emptyAnnotations
  , schemaSelfReference
  )

newtype Markdown a
  = Markdown
  { markdownSchemaDocumentation :: SchemaDocumentation
  }

renderMarkdown :: FC.Schema Markdown a -> LT.Text
renderMarkdown =
  schemaDocumentationToMarkdown . schemaDocumentation

schemaDocumentation :: FC.Schema Markdown a -> SchemaDocumentation
schemaDocumentation =
  markdownSchemaDocumentation . FC.schemaInterpreter

instance FC.Fleece Markdown where
  newtype Object Markdown _object _a
    = Object FieldList

  newtype Field Markdown _object _a
    = Field FieldDocumentation

  newtype AdditionalFields Markdown _object _a
    = AdditionalFields FieldDocumentation

  newtype UnionMembers Markdown _allTypes _handledTypes
    = UnionMembers (DList.DList SchemaDocumentation)

  newtype TaggedUnionMembers Markdown _adt _allTags _handledTags
    = TaggedUnionMembers (DList.DList TaggedUnionMemberDocumentation)

  interpretDescribe net schema =
    Markdown $
      let
        Markdown schemaDoc = FC.schemaInterpreter schema
      in
        schemaDoc
          { schemaDescription = Just net
          }

  interpretFormat formatString schema =
    Markdown $
      let
        Markdown schemaDoc = FC.schemaInterpreter schema
        annotations = schemaAnnotations schemaDoc
      in
        schemaDoc
          { schemaAnnotations = annotations {annotationFormat = Just formatString}
          }

  interpretNumber =
    primitiveMarkdown

  interpretText =
    primitiveMarkdown

  interpretBoolean =
    primitiveMarkdown

  interpretArray arrayName schema =
    Markdown $
      let
        Markdown itemSchemaDoc = FC.schemaInterpreter schema
      in
        SchemaDocumentation
          { schemaName = arrayName
          , schemaDescription = Nothing
          , schemaExcludeFromRender = True
          , schemaNullability = NotNull
          , schemaAnnotations = emptyAnnotations
          , schemaMainEntry = ArrayEntry (schemaMainEntry itemSchemaDoc)
          , schemaReferences = schemaSelfReference itemSchemaDoc
          }

  interpretNull =
    primitiveMarkdown

  interpretNullable nullableName schema =
    Markdown $
      let
        Markdown schemaDoc = FC.schemaInterpreter schema
      in
        SchemaDocumentation
          { schemaName = nullableName
          , schemaDescription = Nothing
          , schemaExcludeFromRender = schemaExcludeFromRender schemaDoc
          , schemaNullability = Nullable schemaDoc
          , schemaAnnotations = emptyAnnotations
          , schemaMainEntry = NullableEntry (schemaMainEntry schemaDoc)
          , schemaReferences = schemaReferences schemaDoc
          }

  required name _accessor fieldSchema =
    Field (mkFieldDocs name True fieldSchema)

  optional name _accessor fieldSchema =
    Field (mkFieldDocs name False fieldSchema)

  mapField _f =
    coerce

  additionalFields _accessor fieldSchema =
    AdditionalFields (mkFieldDocs "All Other Keys" False fieldSchema)

  constructor _f =
    Object mempty

  field (Object fields) (Field fieldDocs) =
    Object (DList.snoc fields fieldDocs)

  additional (Object fields) (AdditionalFields fieldDocs) =
    Object (DList.snoc fields fieldDocs)

  interpretObjectNamed name (Object fields) =
    Markdown $
      SchemaDocumentation
        { schemaName = name
        , schemaDescription = Nothing
        , schemaExcludeFromRender = False
        , schemaNullability = NotNull
        , schemaAnnotations = emptyAnnotations
        , schemaMainEntry = Fields fields
        , schemaReferences = foldMap (schemaSelfReference . fieldSchemaDocs) fields
        }

  interpretValidateNamed name _check _unvalidate schema =
    Markdown $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
      in
        schemaDocs
          { schemaName = name
          , schemaDescription = Nothing
          , schemaExcludeFromRender = False
          }

  interpretValidateAnonymous _check _unvalidate schema =
    let
      Markdown schemaDocs = FC.schemaInterpreter schema
    in
      Markdown schemaDocs

  interpretBoundedEnumNamed name toText =
    let
      enumValues =
        map toText [minBound .. maxBound]
    in
      Markdown $
        SchemaDocumentation
          { schemaName = name
          , schemaDescription = Nothing
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
          , schemaAnnotations = emptyAnnotations
          , schemaMainEntry = EnumValues enumValues
          , schemaReferences = Map.empty
          }

  interpretUnionNamed name (UnionMembers membersDList) =
    Markdown $
      let
        members =
          DList.toList membersDList
      in
        SchemaDocumentation
          { schemaName = name
          , schemaDescription = Nothing
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
          , schemaAnnotations = emptyAnnotations
          , schemaMainEntry = UnionEntry (map schemaName members)
          , schemaReferences =
              Map.fromList
                . map (\docs -> (schemaName docs, docs))
                $ members
          }

  unionMemberWithIndex _index schema =
    UnionMembers $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
      in
        DList.singleton schemaDocs

  unionCombine (UnionMembers left) (UnionMembers right) =
    UnionMembers (left <> right)

  interpretTaggedUnionNamed name tagProperty (TaggedUnionMembers membersDList) =
    Markdown $
      let
        members =
          DList.toList membersDList

        memberSchemaReferences =
          foldMap (schemaSelfReference . fieldSchemaDocs) . tagFields
      in
        SchemaDocumentation
          { schemaName = name
          , schemaDescription = Nothing
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
          , schemaAnnotations = emptyAnnotations
          , schemaMainEntry = TaggedUnionEntry (T.pack tagProperty) members
          , schemaReferences = foldMap memberSchemaReferences members
          }

  taggedUnionMemberWithTag _tag jsonTagValue object =
    TaggedUnionMembers $
      let
        Object fields = object
      in
        DList.singleton
          TaggedUnionMemberDocumentation
            { tagValue = T.pack jsonTagValue
            , tagFields = fields
            }

  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (left <> right)

  interpretJsonString schema =
    Markdown $
      let
        name = FC.schemaName schema
        Markdown schemaDocs = FC.schemaInterpreter schema
      in
        schemaDocs
          { schemaName = FC.annotateName name "(encoded as json string)"
          }

  interpretMinLength len schema =
    Markdown $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
        annotations = schemaAnnotations schemaDocs
      in
        schemaDocs
          { schemaAnnotations = annotations {annotationMinLength = Just len}
          }

  interpretMaxLength len schema =
    Markdown $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
        annotations = schemaAnnotations schemaDocs
      in
        schemaDocs
          { schemaAnnotations = annotations {annotationMaxLength = Just len}
          }

  interpretMinItems len schema =
    Markdown $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
        annotations = schemaAnnotations schemaDocs
      in
        schemaDocs
          { schemaAnnotations = annotations {annotationMinItems = Just len}
          }

  interpretMaxItems len schema =
    Markdown $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
        annotations = schemaAnnotations schemaDocs
      in
        schemaDocs
          { schemaAnnotations = annotations {annotationMaxItems = Just len}
          }

  interpretMinimum val schema =
    Markdown $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
        annotations = schemaAnnotations schemaDocs
      in
        schemaDocs
          { schemaAnnotations = annotations {annotationMinimum = Just val}
          }

  interpretMaximum val schema =
    Markdown $
      let
        Markdown schemaDocs = FC.schemaInterpreter schema
        annotations = schemaAnnotations schemaDocs
      in
        schemaDocs
          { schemaAnnotations = annotations {annotationMaximum = Just val}
          }

primitiveMarkdown :: FC.Name -> Markdown a
primitiveMarkdown name =
  Markdown $
    SchemaDocumentation
      { schemaName = name
      , schemaDescription = Nothing
      , schemaExcludeFromRender = True
      , schemaNullability = NotNull
      , schemaAnnotations = emptyAnnotations
      , schemaMainEntry = NameOnly name
      , schemaReferences = Map.empty
      }

mkFieldDocs ::
  String ->
  Bool ->
  FC.Schema Markdown a ->
  FieldDocumentation
mkFieldDocs name required schema =
  let
    Markdown schemaDocs = FC.schemaInterpreter schema
    nullAllowed =
      case schemaNullability schemaDocs of
        NotNull -> False
        Nullable _notNullSchema -> True
  in
    FieldDocumentation
      { fieldName = T.pack name
      , fieldKeyRequired = required
      , fieldAllowsNull = nullAllowed
      , fieldSchemaDocs = schemaDocs
      }
