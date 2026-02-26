{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Test.Tasty as Tasty
import Test.Tasty.Golden (goldenVsStringDiff)

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import qualified Fleece.Examples as Examples
import qualified Fleece.Markdown as FM

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "json-fleece-markdown"
      [ testGroup
      ]

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "Markdown Rendering"
    [ test_object
    , test_optional
    , test_optionalNullableFieldEmitNull
    , test_optionalNullableFieldOmitKey
    , test_additional
    , test_boundedEnum
    , test_nullableField
    , test_anyJSON
    , test_format
    , test_validate
    , test_validateNamedFormat
    , test_validatedFields
    , test_formattedFields
    , test_abnormalNumbers
    , test_listField
    , test_union
    , test_taggedUnion
    , test_nestedObject
    , test_nameDisambiguation
    ]

test_object :: Tasty.TestTree
test_object =
  mkGoldenTest
    "An object describes its fields in the rendered Markdown"
    "test/examples/object.md"
    Examples.fooBarSchema

test_nullableField :: Tasty.TestTree
test_nullableField =
  mkGoldenTest
    "Nullable fields are notated propertly in the rendered Markdown"
    "test/examples/nullable.md"
    Examples.nullableFieldSchema

test_optional :: Tasty.TestTree
test_optional =
  mkGoldenTest
    "Optional fields are notated propertly in the rendered Markdown"
    "test/examples/optional.md"
    Examples.optionalFieldSchema

test_optionalNullableFieldEmitNull :: Tasty.TestTree
test_optionalNullableFieldEmitNull =
  mkGoldenTest
    "OptionalNullable EmitNull fields are notated propertly in the rendered Markdown"
    "test/examples/optional-nullable-field-emit-null.md"
    Examples.optionalNullableFieldEmitNullSchema

test_optionalNullableFieldOmitKey :: Tasty.TestTree
test_optionalNullableFieldOmitKey =
  mkGoldenTest
    "OptionalNullable OmitKey fields are notated propertly in the rendered Markdown"
    "test/examples/optional-nullable-field-omit-key.md"
    Examples.optionalNullableFieldOmitKeySchema

test_additional :: Tasty.TestTree
test_additional =
  mkGoldenTest
    "Additional properties are included in the object fields in the rendered Markdown"
    "test/examples/additional.md"
    Examples.additionalFieldsExampleSchema

test_boundedEnum :: Tasty.TestTree
test_boundedEnum =
  mkGoldenTest
    "A bounded Enum fullly describes itself in the rendered Markdown"
    "test/examples/bounded-enum.md"
    Examples.boundedEnumSchema

test_anyJSON :: Tasty.TestTree
test_anyJSON =
  mkGoldenTest
    "AnyJSON fuly describes itself in the rendered Markdown"
    "test/examples/any-json.md"
    FC.anyJSON

test_format :: Tasty.TestTree
test_format =
  mkGoldenTest
    "Formatted primitives are rendered to Markdown"
    "test/examples/format.md"
    (FC.format "abc123" FC.text)

test_validate :: Tasty.TestTree
test_validate =
  mkGoldenTest
    "Validated type at the root are included in the markdown"
    "test/examples/validate.md"
    Examples.validationSchema

test_validateNamedFormat :: Tasty.TestTree
test_validateNamedFormat =
  mkGoldenTest
    "Formatted named types are rendered to Markdown with their name"
    "test/examples/validate-named-format.md"
    (FC.validateNamed "ValidatedFormat" id Right $ FC.format "abc123" FC.text)

test_validatedFields :: Tasty.TestTree
test_validatedFields =
  let
    testSchema =
      FC.objectNamed "SomeObject" $
        FC.constructor (\_ _ -> ())
          #+ FC.required "validateNamedField" (const "") (FC.validateNamed "SomeString" id Right $ FC.text)
          #+ FC.required "validateAnonymousField" (const "") (FC.validateAnonymous id Right $ FC.text)
  in
    mkGoldenTest
      "Variations of validated fields are rendered to Markdown as expected"
      "test/examples/validated-fields.md"
      testSchema

test_formattedFields :: Tasty.TestTree
test_formattedFields =
  let
    testSchema =
      FC.objectNamed "SomeObject" $
        FC.constructor (\_ _ _ -> ())
          #+ FC.required "formattedField" (const "") (FC.format "abc123" FC.text)
          #+ FC.required "validatedFormattedField" (const "") (FC.validateNamed "ValidatedFormattedString" id Right . FC.format "abc123" $ FC.text)
          #+ FC.required "formattedValidatedField" (const "") (FC.format "abc123" . FC.validateNamed "FormattedValidatedString" id Right $ FC.text)
  in
    mkGoldenTest
      "Variations of formatted fields are rendered to Markdown as expected"
      "test/examples/formatted-fields.md"
      testSchema

test_abnormalNumbers :: Tasty.TestTree
test_abnormalNumbers =
  mkGoldenTest
    "Abnormal numbers have their types listed with their fields"
    "test/examples/abnormal-numbers.md"
    Examples.abnormalNumbersExampleSchema

test_listField :: Tasty.TestTree
test_listField =
  mkGoldenTest
    "Lists fields have array type listed with the field and the item schema included in the rendered Markdown"
    "test/examples/test-field.md"
    Examples.listFieldExampleSchema

test_union :: Tasty.TestTree
test_union =
  mkGoldenTest
    "Unions are included with their members in the rendered markdown"
    "test/examples/union.md"
    Examples.unionExampleSchema

test_taggedUnion :: Tasty.TestTree
test_taggedUnion =
  mkGoldenTest
    "Tagged Unions are included with their members in the rendered Markdown"
    "test/examples/tagged-union.md"
    Examples.taggedUnionExampleSchema

test_nestedObject :: Tasty.TestTree
test_nestedObject =
  mkGoldenTest
    "Nested objects are included in the rendered Markdown"
    "test/examples/nested-object.md"
    parentSchema

data Parent = Parent
  { parentField1 :: T.Text
  , parentField2 :: T.Text
  , nestedObject :: NestedObject
  }

parentSchema :: FC.Fleece t => FC.Schema t Parent
parentSchema =
  FC.object $
    FC.constructor Parent
      #+ FC.required "field1" parentField1 FC.text
      #+ FC.required "field2" parentField2 FC.text
      #+ FC.required "nested" nestedObject nestedObjectSchema

data NestedObject = NestedObject
  { nestedField1 :: T.Text
  , nestedField2 :: T.Text
  }

nestedObjectSchema :: FC.Fleece t => FC.Schema t NestedObject
nestedObjectSchema =
  FC.object $
    FC.constructor NestedObject
      #+ FC.required "field1" nestedField1 FC.text
      #+ FC.required "field2" nestedField2 FC.text

test_nameDisambiguation :: Tasty.TestTree
test_nameDisambiguation =
  mkGoldenTest
    "Ambiguous Names get qualified in rendered Markdown"
    "test/examples/name-disambiguation.md"
    ambiguousNameParentSchema

data AmbiguousNameParent = AmbiguousNameParent
  { ambiguousNameChild1 :: AmbiguousNameChild1
  , ambiguousNameChild2 :: AmbiguousNameChild2
  }

ambiguousNameParentSchema :: FC.Fleece t => FC.Schema t AmbiguousNameParent
ambiguousNameParentSchema =
  FC.objectNamed "Parent.AmbiguousNameParent" $
    FC.constructor AmbiguousNameParent
      #+ FC.required "child1" ambiguousNameChild1 ambiguousNameChild1Schema
      #+ FC.required "child2" ambiguousNameChild2 ambiguousNameChild2Schema

newtype AmbiguousNameChild1 = AmbiguousNameChild1
  { ambiguousNameChild1Value :: T.Text
  }

ambiguousNameChild1Schema :: FC.Fleece t => FC.Schema t AmbiguousNameChild1
ambiguousNameChild1Schema =
  FC.objectNamed "Child1.AmbiguousName" $
    FC.constructor AmbiguousNameChild1
      #+ FC.required "value" ambiguousNameChild1Value FC.text

newtype AmbiguousNameChild2 = AmbiguousNameChild2
  { ambiguousNameChild2Value :: T.Text
  }

ambiguousNameChild2Schema :: FC.Fleece t => FC.Schema t AmbiguousNameChild2
ambiguousNameChild2Schema =
  FC.objectNamed "Child2.AmbiguousName" $
    FC.constructor AmbiguousNameChild2
      #+ FC.required "value" ambiguousNameChild2Value FC.text

mkGoldenTest ::
  Tasty.TestName ->
  FilePath ->
  FC.Schema FM.Markdown a ->
  Tasty.TestTree
mkGoldenTest testName goldenPath schema = do
  -- Using VsStringDiff instead of VsString because the output for failing
  -- tests is better
  goldenVsStringDiff
    testName
    (\ref new -> ["diff", "-u", ref, new])
    goldenPath
    (pure . LTE.encodeUtf8 . FM.renderMarkdown $ schema)
