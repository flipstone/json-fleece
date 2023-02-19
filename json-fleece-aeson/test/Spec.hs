{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad (join)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (Scientific, scientific)
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as HHM
import qualified Hedgehog.Range as Range

import qualified Fleece.Aeson as FA
import qualified Fleece.Core as FC
import qualified Fleece.Examples as Examples

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-aeson" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_decode_number", prop_decode_number)
  , ("prop_encode_number", prop_encode_number)
  , ("prop_decode_text", prop_decode_text)
  , ("prop_encode_text", prop_encode_text)
  , ("prop_decode_object", prop_decode_object)
  , ("prop_encode_object", prop_encode_object)
  , ("prop_decode_boundedEnum", prop_decode_boundedEnum)
  , ("prop_encode_boundedEnum", prop_encode_boundedEnum)
  , ("prop_encode_nullableField", prop_encode_nullableField)
  , ("prop_decode_nullableField", prop_decode_nullableField)
  , ("prop_encode_validate", prop_encode_validate)
  , ("prop_decode_validate", prop_decode_validate)
  , ("prop_decode_nullableField_Failure", prop_decode_nullableField_Failure)
  , ("prop_decode_optionalField_EmitNull_AcceptNull", prop_decode_optionalField_EmitNull_AcceptNull)
  , ("prop_encode_optionalField_EmitNull_AcceptNull", prop_encode_optionalField_EmitNull_AcceptNull)
  , ("prop_decode_optionalField_OmitKey_AcceptNull", prop_decode_optionalField_OmitKey_AcceptNull)
  , ("prop_encode_optionalField_OmitKey_AcceptNull", prop_encode_optionalField_OmitKey_AcceptNull)
  , ("prop_decode_optionalField_OmitKey_DelegateNull", prop_decode_optionalField_OmitKey_DelegateNull)
  , ("prop_encode_optionalField_OmitKey_DelegateNull", prop_encode_optionalField_OmitKey_DelegateNull)
  , ("prop_decode_optionalField_OmitKey_DelegateNull_Failure", prop_decode_optionalField_OmitKey_DelegateNull_Failure)
  , ("prop_encode_optionalField_OmitKey_DelegateNull_Nullable", prop_encode_optionalField_OmitKey_DelegateNull_Nullable)
  , ("prop_decode_optionalField_OmitKey_DelegateNull_Nullable", prop_decode_optionalField_OmitKey_DelegateNull_Nullable)
  ]

prop_decode_number :: HH.Property
prop_decode_number =
  HH.property $ do
    num <- HH.forAll genScientific
    FA.decode FC.number (Aeson.encode (Aeson.Number num)) === Right num

prop_encode_number :: HH.Property
prop_encode_number =
  HH.property $ do
    num <- HH.forAll genScientific
    FA.encode FC.number num === Aeson.encode (Aeson.Number num)

prop_decode_text :: HH.Property
prop_decode_text =
  HH.property $ do
    text <- HH.forAll genText
    FA.decode FC.text (Aeson.encode (Aeson.String text)) === Right text

prop_encode_text :: HH.Property
prop_encode_text =
  HH.property $ do
    text <- HH.forAll genText
    FA.encode FC.text text === Aeson.encode (Aeson.String text)

prop_decode_object :: HH.Property
prop_decode_object =
  HH.property $ do
    fooValue <- HH.forAll genText
    barValue <- HH.forAll genScientific

    let
      jsonObject =
        encodeTestObject
          [ "foo" .= (Aeson.String fooValue)
          , "bar" .= (Aeson.Number barValue)
          ]

      expected =
        Examples.FooBar
          { Examples.foo = fooValue
          , Examples.bar = barValue
          }

    FA.decode Examples.fooBarSchema jsonObject === Right expected

prop_encode_object :: HH.Property
prop_encode_object =
  HH.property $ do
    fooValue <- HH.forAll genText
    barValue <- HH.forAll genScientific

    let
      expected =
        encodeTestObject
          [ "foo" .= fooValue
          , "bar" .= barValue
          ]

      fooBar =
        Examples.FooBar
          { Examples.foo = fooValue
          , Examples.bar = barValue
          }

    FA.encode Examples.fooBarSchema fooBar === expected

prop_decode_boundedEnum :: HH.Property
prop_decode_boundedEnum =
  HH.property $ do
    textValue <-
      HH.forAll $
        Gen.choice
          [ pure "apple"
          , pure "orange"
          , pure "kumquat"
          , genText
          ]

    let
      testInput =
        Aeson.encode textValue

      expected =
        case textValue of
          "apple" -> Right Examples.Apple
          "orange" -> Right Examples.Orange
          "kumquat" -> Right Examples.Kumquat
          _ -> Left $ "Error in $: Unrecognized value for BoundedEnumExample enum: " <> show textValue

      decoded =
        FA.decode
          Examples.boundedEnumExampleSchema
          testInput

    decoded === expected

prop_encode_boundedEnum :: HH.Property
prop_encode_boundedEnum =
  HH.property $ do
    enumValue <- HH.forAll Gen.enumBounded

    let
      expected =
        Aeson.encode
          . Examples.boundedEnumExampleToText
          $ enumValue

      encoded =
        FA.encode
          Examples.boundedEnumExampleSchema
          enumValue

    encoded === expected

prop_encode_nullableField :: HH.Property
prop_encode_nullableField =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      encoded =
        FA.encode
          Examples.nullableFieldExampleSchema
          (Examples.NullableFieldExample mbText)

      expected =
        encodeTestObject
          ["nullableField" .= mbText]

    encoded === expected

prop_decode_nullableField :: HH.Property
prop_decode_nullableField =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      testInput =
        encodeTestObject
          ["nullableField" .= mbText]

      decoded =
        FA.decode
          Examples.nullableFieldExampleSchema
          testInput

      expected =
        Right
          . Examples.NullableFieldExample
          $ mbText

    decoded === expected

prop_encode_validate :: HH.Property
prop_encode_validate =
  HH.property $ do
    text <- HH.forAll genText

    let
      encoded =
        FA.encode
          Examples.validationExampleSchema
          (Examples.ValidationExample text)

      expected =
        Aeson.encode text

    encoded === expected

prop_decode_validate :: HH.Property
prop_decode_validate =
  HH.property $ do
    text <- HH.forAll genText

    let
      testInput =
        Aeson.encode text

      decoded =
        FA.decode
          Examples.validationExampleSchema
          testInput

      expected =
        if T.length text > 12
          then Left "Error in $: Error validating ValidationExample: At most 12 characters allowed"
          else Right (Examples.ValidationExample text)

    decoded === expected

prop_decode_nullableField_Failure :: HH.Property
prop_decode_nullableField_Failure =
  HH.withTests 1 . HH.property $ do
    let
      testInput =
        encodeTestObject []

      decoded =
        FA.decode
          Examples.nullableFieldExampleSchema
          testInput

      expected =
        Left "Error in $: key \"nullableField\" not found"

    decoded === expected

prop_encode_optionalField_EmitNull_AcceptNull :: HH.Property
prop_encode_optionalField_EmitNull_AcceptNull =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      encoded =
        FA.encode
          Examples.optionalField_EmitNull_AcceptNull_ExampleSchema
          (Examples.OptionalField_EmitNull_AcceptNull_Example mbText)

      expected =
        encodeTestObject
          ["optional_EmitNull_AcceptNull_Field" .= mbText]

    encoded === expected

prop_decode_optionalField_EmitNull_AcceptNull :: HH.Property
prop_decode_optionalField_EmitNull_AcceptNull =
  HH.property $ do
    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      testInput =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["optional_EmitNull_AcceptNull_Field" .= mbText]
            Nothing -> []

      decoded =
        FA.decode
          Examples.optionalField_EmitNull_AcceptNull_ExampleSchema
          testInput

      expected =
        Right
          . Examples.OptionalField_EmitNull_AcceptNull_Example
          . join
          $ mbMbText

    decoded === expected

prop_encode_optionalField_OmitKey_AcceptNull :: HH.Property
prop_encode_optionalField_OmitKey_AcceptNull =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      encoded =
        FA.encode
          Examples.optionalField_OmitKey_AcceptNull_ExampleSchema
          (Examples.OptionalField_OmitKey_AcceptNull_Example mbText)

      expected =
        encodeTestObject $
          case mbText of
            Just text -> ["optional_OmitKey_AcceptNull_Field" .= text]
            Nothing -> []

    encoded === expected

prop_decode_optionalField_OmitKey_AcceptNull :: HH.Property
prop_decode_optionalField_OmitKey_AcceptNull =
  HH.property $ do
    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      testInput =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["optional_OmitKey_AcceptNull_Field" .= mbText]
            Nothing -> []

      decoded =
        FA.decode
          Examples.optionalField_OmitKey_AcceptNull_ExampleSchema
          testInput

      expected =
        Right
          . Examples.OptionalField_OmitKey_AcceptNull_Example
          . join
          $ mbMbText

    decoded === expected

prop_encode_optionalField_OmitKey_DelegateNull :: HH.Property
prop_encode_optionalField_OmitKey_DelegateNull =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      encoded =
        FA.encode
          Examples.optionalField_OmitKey_DelegateNull_ExampleSchema
          (Examples.OptionalField_OmitKey_DelegateNull_Example mbText)

      expected =
        encodeTestObject $
          case mbText of
            Just text -> ["optional_OmitKey_DelegateNull_Field" .= text]
            Nothing -> []

    encoded === expected

prop_decode_optionalField_OmitKey_DelegateNull :: HH.Property
prop_decode_optionalField_OmitKey_DelegateNull =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      testInput =
        encodeTestObject $
          case mbText of
            Just text -> ["optional_OmitKey_DelegateNull_Field" .= text]
            Nothing -> []

      decoded =
        FA.decode
          Examples.optionalField_OmitKey_DelegateNull_ExampleSchema
          testInput

      expected =
        Right
          . Examples.OptionalField_OmitKey_DelegateNull_Example
          $ mbText

    decoded === expected

prop_decode_optionalField_OmitKey_DelegateNull_Failure :: HH.Property
prop_decode_optionalField_OmitKey_DelegateNull_Failure =
  HH.withTests 1 . HH.property $ do
    let
      testInput =
        encodeTestObject
          [ "optional_OmitKey_DelegateNull_Field" .= (Nothing :: Maybe T.Text)
          ]

      decoded =
        FA.decode
          Examples.optionalField_OmitKey_DelegateNull_ExampleSchema
          testInput

      expected =
        Left "Error in $['optional_OmitKey_DelegateNull_Field']: parsing text failed, expected String, but encountered Null"

    decoded === expected

prop_encode_optionalField_OmitKey_DelegateNull_Nullable :: HH.Property
prop_encode_optionalField_OmitKey_DelegateNull_Nullable =
  HH.property $ do
    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      encoded =
        FA.encode
          Examples.optionalField_OmitKey_DelegateNull_NullableExampleSchema
          (Examples.OptionalField_OmitKey_DelegateNull_NullableExample mbMbText)

      expected =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["optional_OmitKey_DelegateNull_Nullable_Field" .= mbText]
            Nothing -> []

    encoded === expected

prop_decode_optionalField_OmitKey_DelegateNull_Nullable :: HH.Property
prop_decode_optionalField_OmitKey_DelegateNull_Nullable =
  HH.property $ do
    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      testInput =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["optional_OmitKey_DelegateNull_Nullable_Field" .= mbText]
            Nothing -> []

      decoded =
        FA.decode
          Examples.optionalField_OmitKey_DelegateNull_NullableExampleSchema
          testInput

      expected =
        Right
          . Examples.OptionalField_OmitKey_DelegateNull_NullableExample
          $ mbMbText

    decoded === expected

encodeTestObject :: [AesonEncoding.Series] -> LBS.ByteString
encodeTestObject =
  AesonEncoding.encodingToLazyByteString
    . Aeson.pairs
    . mconcat

genScientific :: HH.Gen Scientific
genScientific =
  scientific
    <$> Gen.integral (Range.linearFrom 0 (-10000) 10000)
    <*> Gen.integral (Range.linearFrom 0 minBound maxBound)

genText :: HH.Gen T.Text
genText =
  Gen.text (Range.linear 0 32) Gen.unicodeAll
