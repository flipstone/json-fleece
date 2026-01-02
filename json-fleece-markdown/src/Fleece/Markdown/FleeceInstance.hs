{-# LANGUAGE TypeFamilies #-}

module Fleece.Markdown.FleeceInstance
  ( Markdown
  , renderMarkdown
  ) where

import Data.Coerce (coerce)
import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.TypeLits (symbolVal)

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
  , MainEntry (ArrayEntry, EnumValues, Fields, NameOnly, NullableEntry, TaggedUnionEntry, UnionEntry, WithFormat)
  , SchemaDocumentation
    ( SchemaDocumentation
    , schemaExcludeFromRender
    , schemaMainEntry
    , schemaName
    , schemaNullability
    , schemaReferences
    )
  , SchemaNullability (NotNull, Nullable)
  , TaggedUnionMemberDocumentation (TaggedUnionMemberDocumentation, tagFields, tagValue)
  , schemaSelfReference
  )

newtype Markdown a = Markdown SchemaDocumentation

renderMarkdown :: FC.Schema Markdown a -> LT.Text
renderMarkdown (FC.Schema _ (Markdown schemaDoc)) =
  schemaDocumentationToMarkdown schemaDoc

instance FC.Fleece Markdown where
  newtype Object Markdown _object _a
    = Object FieldList

  newtype Field Markdown _object _a
    = Field FieldDocumentation

  newtype AdditionalFields Markdown _object _a
    = AdditionalFields FieldDocumentation

  newtype UnionMembers Markdown _allTypes _handledTypes
    = UnionMembers (DList.DList SchemaDocumentation)

  newtype TaggedUnionMembers Markdown _allTags _handledTags
    = TaggedUnionMembers (DList.DList TaggedUnionMemberDocumentation)

  interpretFormat formatString (FC.Schema _name (Markdown schemaDoc)) =
    Markdown $
      schemaDoc
        { schemaMainEntry = WithFormat formatString (schemaMainEntry schemaDoc)
        }

  interpretNumber =
    primitiveMarkdown

  interpretText =
    primitiveMarkdown

  interpretBoolean =
    primitiveMarkdown

  interpretArray arrayName (FC.Schema _itemName (Markdown itemSchemaDoc)) =
    Markdown $
      SchemaDocumentation
        { schemaName = arrayName
        , schemaExcludeFromRender = True
        , schemaNullability = NotNull
        , schemaMainEntry = ArrayEntry (schemaMainEntry itemSchemaDoc)
        , schemaReferences = schemaSelfReference itemSchemaDoc
        }

  interpretNull =
    primitiveMarkdown

  interpretNullable nullableName (FC.Schema _name (Markdown schemaDoc)) =
    Markdown $
      SchemaDocumentation
        { schemaName = nullableName
        , schemaExcludeFromRender = schemaExcludeFromRender schemaDoc
        , schemaNullability = Nullable schemaDoc
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
        , schemaExcludeFromRender = False
        , schemaNullability = NotNull
        , schemaMainEntry = Fields fields
        , schemaReferences = foldMap (schemaSelfReference . fieldSchemaDocs) fields
        }

  interpretValidateNamed name _check _unvalidate (FC.Schema _unvalidatedName (Markdown schemaDocs)) =
    Markdown $
      schemaDocs
        { schemaName = name
        , schemaExcludeFromRender = False
        }

  interpretValidateAnonymous _check _unvalidate (FC.Schema _name (Markdown schemaDocs)) =
    Markdown schemaDocs

  interpretBoundedEnumNamed name toText =
    let
      enumValues =
        map toText [minBound .. maxBound]
    in
      Markdown $
        SchemaDocumentation
          { schemaName = name
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
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
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
          , schemaMainEntry = UnionEntry (map schemaName members)
          , schemaReferences =
              Map.fromList
                . map (\docs -> (schemaName docs, docs))
                $ members
          }

  unionMemberWithIndex _index (FC.Schema _name markdown) =
    UnionMembers $
      let
        Markdown schemaDocs = markdown
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
          , schemaExcludeFromRender = False
          , schemaNullability = NotNull
          , schemaMainEntry = TaggedUnionEntry (T.pack tagProperty) members
          , schemaReferences = foldMap memberSchemaReferences members
          }

  taggedUnionMemberWithTag tag object =
    TaggedUnionMembers $
      let
        Object fields = object
      in
        DList.singleton
          TaggedUnionMemberDocumentation
            { tagValue = T.pack (symbolVal tag)
            , tagFields = fields
            }

  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (left <> right)

  interpretJsonString (FC.Schema name (Markdown schemaDocs)) =
    Markdown $
      schemaDocs
        { schemaName = FC.annotateName name "(encoded as json string)"
        }

primitiveMarkdown :: FC.Name -> Markdown a
primitiveMarkdown name =
  Markdown $
    SchemaDocumentation
      { schemaName = name
      , schemaExcludeFromRender = True
      , schemaNullability = NotNull
      , schemaMainEntry = NameOnly name
      , schemaReferences = Map.empty
      }

mkFieldDocs ::
  String ->
  Bool ->
  FC.Schema Markdown a ->
  FieldDocumentation
mkFieldDocs name required (FC.Schema _schemaName (Markdown schemaDocs)) =
  let
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
