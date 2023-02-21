{-# LANGUAGE TypeFamilies #-}

module Fleece.Markdown
  ( Markdown
  , renderMarkdown
  ) where

-- import Data.Coerce (coerce)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

import qualified Fleece.Core as FC

data Markdown a = Markdown
  { schemaNullability :: SchemaNullability
  , schemaDocs :: LTB.Builder
  }

mkMarkdown :: LTB.Builder -> Markdown a
mkMarkdown builder =
  Markdown
    { schemaNullability = NotNull
    , schemaDocs = builder
    }

markNullable :: Markdown a -> Markdown (Maybe a)
markNullable markdown =
  markdown
    { schemaNullability = Nullable
    }

data SchemaNullability
  = NotNull
  | Nullable

renderMarkdown :: Markdown a -> LT.Text
renderMarkdown (Markdown _ builder) = LTB.toLazyText builder

instance FC.Fleece Markdown where
  newtype Object Markdown _object _a = Object LTB.Builder
  newtype Field Markdown _object _a = Field LTB.Builder
  newtype EmbeddedObject Markdown _object _a = EmbeddedObject LTB.Builder

  number =
    mkMarkdown (LTB.fromString "number")

  text =
    mkMarkdown (LTB.fromString "string")

  boolean =
    mkMarkdown (LTB.fromString "boolean")

  array itemSchema =
    mkMarkdown $
      LTB.fromString "array of "
        <> schemaDocs itemSchema
        <> case schemaNullability itemSchema of
          NotNull -> mempty
          Nullable -> LTB.fromString " (nullable)"

  null =
    mkMarkdown (LTB.fromString "null")

  nullable =
    markNullable

  required name _accessor fieldSchema =
    Field (fieldRow name Nothing fieldSchema)

  optionalField nullBehavior name _accessor fieldSchema =
    Field (fieldRow name (Just nullBehavior) fieldSchema)

  constructor _f =
    Object mempty

  field (Object fieldsMarkdown) (Field moreMarkdown) =
    Object (fieldsMarkdown <> moreMarkdown)

  embed (Object fieldsMarkdown) (EmbeddedObject embeddedFieldsMarkdown) =
    Object (fieldsMarkdown <> embeddedFieldsMarkdown)

  embedded _accessor (Object embeddedFieldsMarkdown) =
    EmbeddedObject embeddedFieldsMarkdown

  objectNamed name (Object fieldsMarkdown) =
    let
      markdown =
        mkMarkdown $
          h1 name
            <> newline
            <> newline
            <> fieldsHeader
            <> fieldsMarkdown
    in
      markdown

  validateNamed name _check _unvalidate markdown =
    markdown
      { schemaDocs =
          h1 name
            <> newline
            <> newline
            <> schemaDocs markdown
            <> LTB.fromString " (with validation restrictions)"
            <> newline
      }

  boundedEnumNamed name toText =
    let
      enumValues =
        foldMap
          (listItem . LTB.fromText . toText)
          [minBound .. maxBound]
    in
      mkMarkdown $
        h1 name
          <> newline
          <> newline
          <> LTB.fromString "Enum values:"
          <> newline
          <> newline
          <> enumValues

h1 :: String -> LTB.Builder
h1 str =
  LTB.fromString "# " <> LTB.fromString str

fieldsHeader :: LTB.Builder
fieldsHeader =
  LTB.fromString "|Field|Key Required|Null Allowed|Type|"
    <> newline
    <> LTB.fromString "|---|---|---|---|"
    <> newline

fieldRow :: String -> Maybe FC.NullBehavior -> Markdown a -> LTB.Builder
fieldRow name mbNullBehavior fieldSchema =
  let
    doesSchemaAllowNull =
      case schemaNullability fieldSchema of
        NotNull -> no
        Nullable -> yes

    required =
      case mbNullBehavior of
        Nothing -> yes
        Just _ -> no

    nullAllowed =
      case mbNullBehavior of
        Nothing -> doesSchemaAllowNull
        Just FC.EmitNull_AcceptNull -> yes
        Just FC.OmitKey_AcceptNull -> yes
        Just FC.OmitKey_DelegateNull -> doesSchemaAllowNull
  in
    pipe
      <> LTB.fromString name
      <> pipe
      <> required
      <> pipe
      <> nullAllowed
      <> pipe
      <> schemaDocs fieldSchema
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

listItem :: LTB.Builder -> LTB.Builder
listItem item =
  LTB.fromString "- " <> item <> newline
