{-# LANGUAGE TypeFamilies #-}
module Fleece.Markdown
  ( Markdown
  , renderMarkdown
  ) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Typeable (tyConName, typeRepTyCon, typeRep)

import qualified Fleece.Core as FC

newtype Markdown a =
  Markdown LTB.Builder

renderMarkdown :: Markdown a -> LT.Text
renderMarkdown (Markdown builder) = LTB.toLazyText builder

instance FC.Fleece Markdown where
  newtype Object Markdown _object _a = Object LTB.Builder
  newtype Field Markdown _object _a = Field LTB.Builder
  number =
    Markdown (LTB.fromString "number")

  text =
    Markdown (LTB.fromString "string")

  required name _accessor (Markdown builder) =
    Field $
      pipe
        <> LTB.fromString name <> pipe
        <> LTB.fromString "yes" <> pipe
        <> LTB.fromString "no" <> pipe
        <> builder <> pipe
        <> newline

  optionalField _nullBehavior _name _accessor (Markdown _builder) =
    Field mempty

  constructor _f =
    Object mempty

  field (Object fieldsMarkdown) (Field moreMarkdown) =
    Object (fieldsMarkdown <> moreMarkdown)

  object (Object fieldsMarkdown) =
    let
      typeConstructor =
        typeRepTyCon
        . typeRep
        $ markdown

      markdown =
        Markdown $
          h1 (tyConName typeConstructor) <> newline
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

pipe :: LTB.Builder
pipe =
  LTB.fromString "|"

newline :: LTB.Builder
newline =
  LTB.fromString "\n"
