{-# LANGUAGE TypeFamilies #-}

module Fleece.Markdown
  ( Markdown
  , renderMarkdown
  ) where

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
  number =
    mkMarkdown (LTB.fromString "number")

  text =
    mkMarkdown (LTB.fromString "string")

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
