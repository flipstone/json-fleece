{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.Stack (withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM
import qualified Shrubbery

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import qualified Fleece.Examples as Examples
import qualified Fleece.PrettyPrint as FPP

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-pretty-print" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_object", prop_object)
  , ("prop_boundedEnum", prop_boundedEnum)
  , ("prop_nullableField", prop_nullableField)
  , ("prop_jsonText", prop_jsonText)
  , ("prop_jsonBoolean", prop_jsonBoolean)
  , ("prop_jsonNumber", prop_jsonNumber)
  , ("prop_jsonInteger", prop_jsonInteger)
  , ("prop_jsonEmptyArray", prop_jsonEmptyArray)
  , ("prop_jsonStringArray", prop_jsonStringArray)
  , ("prop_jsonObjectArray", prop_jsonObjectArray)
  , ("prop_jsonObject", prop_jsonObject)
  , ("prop_jsonNull", prop_jsonNull)
  , ("prop_union", prop_union)
  , ("prop_taggedUnion", prop_taggedUnion)
  , ("prop_validate", prop_validate)
  , ("prop_optional", prop_optional)
  , ("prop_optionalNullableFieldEmitNull", prop_optionalNullableFieldEmitNull)
  , ("prop_optionalNullableFieldOmitKey", prop_optionalNullableFieldOmitKey)
  , ("prop_additional", prop_additional)
  , ("prop_nestedObject", prop_nestedObject)
  , ("prop_abnormalNumbers", prop_abnormalNumbers)
  ]

prop_object :: HH.Property
prop_object =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.FooBar
          { Examples.foo = "baz"
          , Examples.bar = 1.2
          }

      expected =
        [ "FooBar"
        , "  foo = \"baz\""
        , "  bar = 1.2"
        ]
    in
      assertPrettyPrintEquals Examples.fooBarSchema value expected

prop_boundedEnum :: HH.Property
prop_boundedEnum =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.Apple
      expected =
        [ "\"apple\""
        ]
    in
      assertPrettyPrintEquals Examples.boundedEnumSchema value expected

prop_nullableField :: HH.Property
prop_nullableField =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.NullableField
          { Examples.exampleNullableField = Right "foo"
          }

      expected =
        [ "NullableField"
        , "  nullableField = Right \"foo\""
        ]
    in
      assertPrettyPrintEquals Examples.nullableFieldSchema value expected

prop_jsonText :: HH.Property
prop_jsonText =
  HH.withTests 1 . HH.property $
    assertPrettyPrintEquals
      FC.anyJSON
      (FC.mkJSONText "foo")
      [ "AnyJSON \"foo\""
      ]

prop_jsonBoolean :: HH.Property
prop_jsonBoolean =
  HH.withTests 1 . HH.property $
    assertPrettyPrintEquals
      FC.anyJSON
      (FC.mkJSONBool True)
      [ "AnyJSON True"
      ]

prop_jsonNumber :: HH.Property
prop_jsonNumber =
  HH.withTests 1 . HH.property $
    assertPrettyPrintEquals
      FC.anyJSON
      (FC.mkJSONNumber 1.33)
      [ "AnyJSON 1.33"
      ]

prop_jsonInteger :: HH.Property
prop_jsonInteger =
  HH.withTests 1 . HH.property $
    assertPrettyPrintEquals
      FC.anyJSON
      (FC.mkJSONNumber 3)
      [ "AnyJSON 3"
      ]

prop_jsonEmptyArray :: HH.Property
prop_jsonEmptyArray =
  HH.withTests 1 . HH.property $
    let
      value =
        FC.mkJSONArray []

      expected =
        [ "AnyJSON []"
        ]
    in
      assertPrettyPrintEquals FC.anyJSON value expected

prop_jsonStringArray :: HH.Property
prop_jsonStringArray =
  HH.withTests 1 . HH.property $
    let
      value =
        FC.mkJSONArray [FC.mkJSONText "foo", FC.mkJSONText "bar"]

      expected =
        [ "AnyJSON"
        , "  - AnyJSON \"foo\""
        , "  - AnyJSON \"bar\""
        ]
    in
      assertPrettyPrintEquals FC.anyJSON value expected

prop_jsonObjectArray :: HH.Property
prop_jsonObjectArray =
  HH.withTests 1 . HH.property $
    let
      value =
        FC.mkJSONArray
          [ FC.mkJSONObject (Map.singleton "foo" (FC.mkJSONText "bar"))
          , FC.mkJSONObject (Map.singleton "baz" (FC.mkJSONText "bat"))
          ]

      expected =
        [ "AnyJSON"
        , "  - AnyJSON"
        , "      AnyJSON object"
        , "        -- additional fields --"
        , "        foo = AnyJSON \"bar\""
        , "  - AnyJSON"
        , "      AnyJSON object"
        , "        -- additional fields --"
        , "        baz = AnyJSON \"bat\""
        ]
    in
      assertPrettyPrintEquals FC.anyJSON value expected

prop_jsonObject :: HH.Property
prop_jsonObject =
  HH.withTests 1 . HH.property $
    let
      value =
        FC.mkJSONObject $
          Map.fromList
            [ ("foo", FC.mkJSONText "bar")
            , ("baz", FC.mkJSONText "bat")
            ]

      expected =
        [ "AnyJSON"
        , "  AnyJSON object"
        , "    -- additional fields --"
        , "    baz = AnyJSON \"bat\""
        , "    foo = AnyJSON \"bar\""
        ]
    in
      assertPrettyPrintEquals FC.anyJSON value expected

prop_jsonNull :: HH.Property
prop_jsonNull =
  HH.withTests 1 . HH.property $
    assertPrettyPrintEquals
      FC.anyJSON
      FC.mkJSONNull
      [ "AnyJSON Null"
      ]

prop_union :: HH.Property
prop_union =
  HH.withTests 1 . HH.property $
    let
      value =
        Shrubbery.unify @T.Text "foo"

      expected =
        [ "\"foo\""
        ]
    in
      assertPrettyPrintEquals Examples.unionExampleSchema value expected

prop_taggedUnion :: HH.Property
prop_taggedUnion =
  HH.withTests 1 . HH.property $
    let
      value =
        Shrubbery.unifyTaggedUnion @"person" $
          Examples.Person
            { Examples.personName = "Alice"
            , Examples.personAge = 42
            }

      expected =
        [ "TaggedUnionExample"
        , "  type = \"person\""
        , "  name = \"Alice\""
        , "  age = 42"
        ]
    in
      assertPrettyPrintEquals Examples.taggedUnionExampleSchema value expected

prop_validate :: HH.Property
prop_validate =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.Validation "foo"
      expected =
        [ "Validation \"foo\""
        ]
    in
      assertPrettyPrintEquals Examples.validationSchema value expected

prop_optional :: HH.Property
prop_optional =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.OptionalField
          { Examples.exampleOptionalField = Just "foo"
          }

      expected =
        [ "OptionalField"
        , "  optionalField = Just \"foo\""
        ]
    in
      assertPrettyPrintEquals Examples.optionalFieldSchema value expected

prop_optionalNullableFieldEmitNull :: HH.Property
prop_optionalNullableFieldEmitNull =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.OptionalNullableFieldEmitNull
          { Examples.exampleOptionalNullableFieldEmitNullField = Just "foo"
          }

      expected =
        [ "OptionalNullableFieldEmitNull"
        , "  optionalNullableField = Just (Right \"foo\")"
        ]
    in
      assertPrettyPrintEquals
        Examples.optionalNullableFieldEmitNullSchema
        value
        expected

prop_optionalNullableFieldOmitKey :: HH.Property
prop_optionalNullableFieldOmitKey =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.OptionalNullableFieldOmitKey
          { Examples.exampleOptionalNullableFieldOmitKeyField = Just "foo"
          }

      expected =
        [ "OptionalNullableFieldOmitKey"
        , "  optionalNullableField = Just (Right \"foo\")"
        ]
    in
      assertPrettyPrintEquals Examples.optionalNullableFieldOmitKeySchema value expected

prop_additional :: HH.Property
prop_additional =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.AdditionalFieldsExample
          { Examples.field1 = "foo"
          , Examples.field2 = "bar"
          , Examples.otherFields =
              Map.fromList
                [ ("other1", "baz")
                , ("other2", "bat")
                ]
          }
      expected =
        [ "AdditionalFieldsExample"
        , "  field1 = \"foo\""
        , "  field2 = \"bar\""
        , "  -- additional fields --"
        , "  other1 = \"baz\""
        , "  other2 = \"bat\""
        ]
    in
      assertPrettyPrintEquals Examples.additionalFieldsExampleSchema value expected

prop_abnormalNumbers :: HH.Property
prop_abnormalNumbers =
  HH.withTests 1 . HH.property $
    let
      value =
        Examples.AbnormalNumbersExample
          { Examples.stringyNumber = 3.14
          }

      expected =
        [ "AbnormalNumbersExample"
        , "  stringyNumber = 3.14"
        ]
    in
      assertPrettyPrintEquals Examples.abnormalNumbersExampleSchema value expected

prop_nestedObject :: HH.Property
prop_nestedObject =
  HH.withTests 1 . HH.property $
    let
      value =
        Parent
          { parentField1 = "foo"
          , parentField2 = "bar"
          , nestedObject =
              NestedObject
                { nestedField1 = "baz"
                , nestedField2 = "bat"
                }
          }

      expected =
        [ "Parent"
        , "  field1 = \"foo\""
        , "  field2 = \"bar\""
        , "  nested ="
        , "    NestedObject"
        , "      field1 = \"baz\""
        , "      field2 = \"bat\""
        ]
    in
      assertPrettyPrintEquals parentSchema value expected

data Parent = Parent
  { parentField1 :: T.Text
  , parentField2 :: T.Text
  , nestedObject :: NestedObject
  }

parentSchema :: FC.Fleece schema => schema Parent
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

nestedObjectSchema :: FC.Fleece schema => schema NestedObject
nestedObjectSchema =
  FC.object $
    FC.constructor NestedObject
      #+ FC.required "field1" nestedField1 FC.text
      #+ FC.required "field2" nestedField2 FC.text

assertPrettyPrintEquals :: FPP.PrettyPrinter a -> a -> [LT.Text] -> HH.PropertyT IO ()
assertPrettyPrintEquals schema a expected =
  withFrozenCallStack $
    LT.lines (FPP.prettyPrintLazyText schema a) === expected
