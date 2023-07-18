{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad (join)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Text as AesonText
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, scientific)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Time as Time
import qualified Data.Vector as V
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
  HHM.defaultMain $ [HH.checkParallel (HH.Group "json-fleece-aeson - json" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_decode_number", prop_decode_number)
  , ("prop_encode_number", prop_encode_number)
  , ("prop_decode_string", prop_decode_string)
  , ("prop_encode_string", prop_encode_string)
  , ("prop_decode_boolean", prop_decode_boolean)
  , ("prop_encode_boolean", prop_encode_boolean)
  , ("prop_decode_null", prop_decode_null)
  , ("prop_encode_null", prop_encode_null)
  , ("prop_decode_array", prop_decode_array)
  , ("prop_encode_array", prop_encode_array)
  , ("prop_decode_object", prop_decode_object)
  , ("prop_encode_object", prop_encode_object)
  , ("prop_decode_boundedEnum", prop_decode_boundedEnum)
  , ("prop_encode_boundedEnum", prop_encode_boundedEnum)
  , ("prop_encode_nullableField", prop_encode_nullableField)
  , ("prop_decode_nullableField", prop_decode_nullableField)
  , ("prop_encode_validate", prop_encode_validate)
  , ("prop_decode_validate", prop_decode_validate)
  , ("prop_decode_nullableField_Failure", prop_decode_nullableField_Failure)
  , ("prop_decode_optionalNullableFieldEmitNull", prop_decode_optionalNullableFieldEmitNull)
  , ("prop_encode_optionalNullableFieldEmitNull", prop_encode_optionalNullableFieldEmitNull)
  , ("prop_decode_optionalNullableFieldOmitKey", prop_decode_optionalNullableFieldOmitKey)
  , ("prop_encode_optionalNullableFieldOmitKey", prop_encode_optionalNullableFieldOmitKey)
  , ("prop_decode_additional", prop_decode_additional)
  , ("prop_encode_additional", prop_encode_additional)
  , ("prop_decode_anyJSON", prop_decode_anyJSON)
  , ("prop_encode_anyJSON", prop_encode_anyJSON)
  , ("prop_decode_abnormalNumbers", prop_decode_abnormalNumbers)
  , ("prop_encode_abnormalNumbers", prop_encode_abnormalNumbers)
  , ("prop_decode_listField", prop_decode_listField)
  , ("prop_encode_listField", prop_encode_listField)
  , ("prop_utcTimeAndZonedTime", prop_utcTimeAndZonedTime)
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

prop_decode_string :: HH.Property
prop_decode_string =
  HH.property $ do
    text <- HH.forAll genText
    FA.decode FC.text (Aeson.encode (Aeson.String text)) === Right text

prop_encode_string :: HH.Property
prop_encode_string =
  HH.property $ do
    text <- HH.forAll genText
    FA.encode FC.text text === Aeson.encode (Aeson.String text)

prop_decode_boolean :: HH.Property
prop_decode_boolean =
  HH.property $ do
    bool <- HH.forAll Gen.bool
    FA.decode FC.boolean (Aeson.encode (Aeson.Bool bool)) === Right bool

prop_encode_boolean :: HH.Property
prop_encode_boolean =
  HH.property $ do
    bool <- HH.forAll Gen.bool
    FA.encode FC.boolean bool === Aeson.encode (Aeson.Bool bool)

prop_decode_null :: HH.Property
prop_decode_null =
  HH.withTests 1 . HH.property $ do
    FA.decode FC.null (Aeson.encode Aeson.Null) === Right FC.Null

prop_encode_null :: HH.Property
prop_encode_null =
  HH.withTests 1 . HH.property $ do
    FA.encode FC.null FC.Null === Aeson.encode Aeson.Null

prop_decode_array :: HH.Property
prop_decode_array =
  HH.property $ do
    texts <-
      HH.forAll $
        fmap
          V.fromList
          (Gen.list (Range.linear 0 10) genText)

    let
      testInput =
        Aeson.encode
          . Aeson.Array
          . fmap Aeson.toJSON
          $ texts

      expected =
        Right texts

      decoded =
        FA.decode
          (FC.array FC.text)
          testInput

    decoded === expected

prop_encode_array :: HH.Property
prop_encode_array =
  HH.property $ do
    texts <-
      HH.forAll $
        fmap
          V.fromList
          (Gen.list (Range.linear 0 10) genText)

    let
      expected =
        Aeson.encode
          . Aeson.Array
          . fmap Aeson.toJSON
          $ texts

      encoded =
        FA.encode
          (FC.array FC.text)
          texts

    encoded === expected

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
          _ -> Left $ "Error in $: Unrecognized value for Fleece.Examples.BoundedEnum enum: " <> show textValue

      decoded =
        FA.decode
          Examples.boundedEnumSchema
          testInput

    decoded === expected

prop_encode_boundedEnum :: HH.Property
prop_encode_boundedEnum =
  HH.property $ do
    enumValue <- HH.forAll Gen.enumBounded

    let
      expected =
        Aeson.encode
          . Examples.boundedEnumToText
          $ enumValue

      encoded =
        FA.encode
          Examples.boundedEnumSchema
          enumValue

    encoded === expected

prop_encode_nullableField :: HH.Property
prop_encode_nullableField =
  HH.property $ do
    nullOrText <- HH.forAll (Gen.either (pure FC.Null) genText)

    let
      encoded =
        FA.encode
          Examples.nullableFieldSchema
          (Examples.NullableField nullOrText)

      expected =
        encodeTestObject
          ["nullableField" .= either (const Aeson.Null) Aeson.String nullOrText]

    encoded === expected

prop_decode_nullableField :: HH.Property
prop_decode_nullableField =
  HH.property $ do
    nullOrText <- HH.forAll (Gen.either (pure FC.Null) genText)

    let
      testInput =
        encodeTestObject
          ["nullableField" .= either (const Aeson.Null) Aeson.String nullOrText]

      decoded =
        FA.decode
          Examples.nullableFieldSchema
          testInput

      expected =
        Right
          . Examples.NullableField
          $ nullOrText

    decoded === expected

prop_encode_validate :: HH.Property
prop_encode_validate =
  HH.property $ do
    text <- HH.forAll genText

    let
      encoded =
        FA.encode
          Examples.validationSchema
          (Examples.Validation text)

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
          Examples.validationSchema
          testInput

      expected =
        if T.length text > 12
          then Left "Error in $: Error validating Fleece.Examples.Validation: At most 12 characters allowed"
          else Right (Examples.Validation text)

    decoded === expected

prop_decode_nullableField_Failure :: HH.Property
prop_decode_nullableField_Failure =
  HH.withTests 1 . HH.property $ do
    let
      testInput =
        encodeTestObject []

      decoded =
        FA.decode
          Examples.nullableFieldSchema
          testInput

      expected =
        Left "Error in $: key \"nullableField\" not found"

    decoded === expected

prop_encode_optionalNullableFieldEmitNull :: HH.Property
prop_encode_optionalNullableFieldEmitNull =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      encoded =
        FA.encode
          Examples.optionalNullableFieldEmitNullSchema
          (Examples.OptionalNullableFieldEmitNull mbText)

      expected =
        encodeTestObject
          ["optionalNullableField" .= mbText]

    encoded === expected

prop_decode_optionalNullableFieldEmitNull :: HH.Property
prop_decode_optionalNullableFieldEmitNull =
  HH.property $ do
    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      testInput =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["optionalNullableField" .= mbText]
            Nothing -> []

      decoded =
        FA.decode
          Examples.optionalNullableFieldEmitNullSchema
          testInput

      expected =
        Right
          . Examples.OptionalNullableFieldEmitNull
          . join
          $ mbMbText

    decoded === expected

prop_encode_optionalNullableFieldOmitKey :: HH.Property
prop_encode_optionalNullableFieldOmitKey =
  HH.property $ do
    mbText <- HH.forAll (Gen.maybe genText)

    let
      encoded =
        FA.encode
          Examples.optionalNullableFieldOmitKeySchema
          (Examples.OptionalNullableFieldOmitKey mbText)

      expected =
        encodeTestObject $
          case mbText of
            Just text -> ["optionalNullableField" .= text]
            Nothing -> []

    encoded === expected

prop_decode_optionalNullableFieldOmitKey :: HH.Property
prop_decode_optionalNullableFieldOmitKey =
  HH.property $ do
    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      testInput =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["optionalNullableField" .= mbText]
            Nothing -> []

      decoded =
        FA.decode
          Examples.optionalNullableFieldOmitKeySchema
          testInput

      expected =
        Right
          . Examples.OptionalNullableFieldOmitKey
          . join
          $ mbMbText

    decoded === expected

prop_encode_additional :: HH.Property
prop_encode_additional =
  HH.property $ do
    field1 <- HH.forAll genText
    field2 <- HH.forAll genText

    let
      genPair = do
        name <- Gen.filter (\e -> not (elem e ["field1", "field2"])) genText
        value <- genText
        pure (name, value)

    others <- HH.forAll (Gen.map (Range.linear 0 10) genPair)

    let
      input =
        Examples.AdditionalFieldsExample
          { Examples.field1 = field1
          , Examples.field2 = field2
          , Examples.otherFields = others
          }

      encoded =
        FA.encode
          Examples.additionalFieldsExampleSchema
          input

      expected =
        encodeTestObject $
          ( "field1" .= field1
              : "field2" .= field2
              : map (\(k, v) -> AesonKey.fromText k .= v) (Map.toList others)
          )

    encoded === expected

prop_decode_additional :: HH.Property
prop_decode_additional =
  HH.property $ do
    field1 <- HH.forAll genText
    field2 <- HH.forAll genText

    let
      genPair = do
        name <- Gen.filter (\e -> not (elem e ["field1", "field2"])) genText
        value <- genText
        pure (name, value)

    others <- HH.forAll (Gen.map (Range.linear 0 10) genPair)

    let
      testInput =
        encodeTestObject $
          ( "field1" .= field1
              : "field2" .= field2
              : map (\(k, v) -> AesonKey.fromText k .= v) (Map.toList others)
          )

      decoded =
        FA.decode
          Examples.additionalFieldsExampleSchema
          testInput

      expected =
        Right $
          Examples.AdditionalFieldsExample
            { Examples.field1 = field1
            , Examples.field2 = field2
            , Examples.otherFields = others
            }

    decoded === expected

prop_decode_anyJSON :: HH.Property
prop_decode_anyJSON =
  HH.property $ do
    expected <- HH.forAll genAnyJSON

    let
      encoded =
        Aeson.encode (FA.anyJSONToValue expected)

      decoded =
        FA.decode FC.anyJSON encoded

    Right expected === decoded

prop_encode_anyJSON :: HH.Property
prop_encode_anyJSON =
  HH.property $ do
    anyJSON <- HH.forAll genAnyJSON

    let
      expected =
        Aeson.encode (FA.anyJSONToValue anyJSON)

      encoded =
        FA.encode FC.anyJSON anyJSON

    expected === encoded

prop_decode_abnormalNumbers :: HH.Property
prop_decode_abnormalNumbers =
  HH.property $ do
    stringyNumber <- HH.forAll genScientific
    bareOrStringyNumber <- HH.forAll genScientific
    encodeBare <- HH.forAll Gen.bool

    let
      encoded =
        encodeTestObject
          [ "stringyNumber" .= Aeson.String (encodeAesonText stringyNumber)
          , "bareOrStringyNumber"
              .= if encodeBare
                then Aeson.Number bareOrStringyNumber
                else Aeson.String (encodeAesonText bareOrStringyNumber)
          ]

      decoded =
        FA.decode Examples.abnormalNumbersExampleSchema encoded

      expected =
        Examples.AbnormalNumbersExample
          { Examples.stringyNumber = stringyNumber
          , Examples.bareOrStringyNumber = bareOrStringyNumber
          }

    Right expected === decoded

prop_encode_abnormalNumbers :: HH.Property
prop_encode_abnormalNumbers =
  HH.property $ do
    stringyNumber <- HH.forAll genScientific
    bareOrStringyNumber <- HH.forAll genScientific

    let
      input =
        Examples.AbnormalNumbersExample
          { Examples.stringyNumber = stringyNumber
          , Examples.bareOrStringyNumber = bareOrStringyNumber
          }

      expected =
        encodeTestObject
          [ "stringyNumber" .= Aeson.String (encodeAesonText stringyNumber)
          , "bareOrStringyNumber" .= Aeson.Number bareOrStringyNumber
          ]

      encoded =
        FA.encode Examples.abnormalNumbersExampleSchema input

    expected === encoded

prop_decode_listField :: HH.Property
prop_decode_listField =
  HH.property $ do
    values <- HH.forAll (Gen.list (Range.linear 0 10) Gen.enumBounded)

    let
      jsonObject =
        encodeTestObject
          [ "listField" .= map Examples.boundedEnumToText values
          ]

      expected =
        Examples.ListFieldExample
          { Examples.listField = values
          }

    FA.decode Examples.listFieldExampleSchema jsonObject === Right expected

prop_encode_listField :: HH.Property
prop_encode_listField =
  HH.property $ do
    values <- HH.forAll (Gen.list (Range.linear 0 10) Gen.enumBounded)

    let
      expected =
        encodeTestObject
          [ "listField" .= map Examples.boundedEnumToText values
          ]

      input =
        Examples.ListFieldExample
          { Examples.listField = values
          }

    FA.encode Examples.listFieldExampleSchema input === expected

prop_utcTimeAndZonedTime :: HH.Property
prop_utcTimeAndZonedTime =
  HH.property $ do
    utcOrZonedTime <-
      HH.forAll $ Gen.either genUTCTime genZonedTime

    let
      encoded =
        case utcOrZonedTime of
          Left utcTime -> FA.encode FC.utcTime utcTime
          Right zonedTime -> FA.encode FC.zonedTime zonedTime

      -- we only compare the minutes offset of the time zone because
      -- otherwise UTC and +0000 are not equal
      zonedTimeToTuple zonedTime =
        ( Time.zonedTimeToLocalTime zonedTime
        , Time.timeZoneMinutes (Time.zonedTimeZone zonedTime)
        )

    utcOrZonedTimeDecoded <-
      HH.forAll $
        Gen.either
          (pure $ FA.decode FC.utcTime encoded)
          (pure $ FA.decode FC.zonedTime encoded)

    case (utcOrZonedTime, utcOrZonedTimeDecoded) of
      (Left originalUTCTime, Left decodedUTCTime) ->
        Right originalUTCTime === decodedUTCTime
      (Left originalUTCTime, Right decodedZonedTime) ->
        Right (zonedTimeToTuple (Time.utcToZonedTime Time.utc originalUTCTime))
          === fmap zonedTimeToTuple decodedZonedTime
      (Right originalZonedTime, Right decodedZonedTime) -> do
        Right (zonedTimeToTuple originalZonedTime)
          === fmap zonedTimeToTuple decodedZonedTime
      (Right originalZonedTime, Left decodedUTCTime) -> do
        Right (Time.zonedTimeToUTC originalZonedTime)
          === decodedUTCTime

genAnyJSON :: HH.Gen FC.AnyJSON
genAnyJSON =
  Gen.choice
    [ fmap FC.mkJSONText genText
    , fmap FC.mkJSONBool Gen.bool
    , fmap FC.mkJSONNumber genScientific
    , fmap FC.mkJSONArray (Gen.list (Range.linear 0 5) genAnyJSON)
    , fmap FC.mkJSONObject (Gen.map (Range.linear 0 5) ((,) <$> genText <*> genAnyJSON))
    , pure FC.mkJSONNull
    ]

encodeTestObject :: [AesonEncoding.Series] -> LBS.ByteString
encodeTestObject =
  AesonEncoding.encodingToLazyByteString
    . Aeson.pairs
    . mconcat

encodeAesonText :: (Aeson.ToJSON a) => a -> T.Text
encodeAesonText =
  LT.toStrict . AesonText.encodeToLazyText

genScientific :: HH.Gen Scientific
genScientific =
  scientific
    <$> Gen.integral (Range.linearFrom 0 (-10000) 10000)
    <*> Gen.integral (Range.linearFrom 0 minBound maxBound)

genText :: HH.Gen T.Text
genText =
  Gen.text (Range.linear 0 32) Gen.unicodeAll

genUTCTime :: HH.Gen Time.UTCTime
genUTCTime =
  Time.UTCTime <$> genDay <*> genDiffTime

genZonedTime :: HH.Gen Time.ZonedTime
genZonedTime =
  Time.ZonedTime <$> genLocalTime <*> genTimeZone

genLocalTime :: HH.Gen Time.LocalTime
genLocalTime =
  Time.LocalTime <$> genDay <*> genTimeOfDay

genTimeZone :: HH.Gen Time.TimeZone
genTimeZone =
  Time.minutesToTimeZone <$> Gen.integral (Range.linearFrom 0 (-60 * 12) (60 * 14))

genDay :: HH.Gen Time.Day
genDay = do
  year <- Gen.integral (Range.linearFrom 2000 0 3000)
  month <- Gen.integral (Range.constant 1 12)
  day <- Gen.integral (Range.constant 1 (Time.gregorianMonthLength year month))

  pure (Time.fromGregorian year month day)

genTimeOfDay :: HH.Gen Time.TimeOfDay
genTimeOfDay = fmap Time.timeToTimeOfDay genDiffTime

genDiffTime :: HH.Gen Time.DiffTime
genDiffTime =
  Time.secondsToDiffTime <$> Gen.integral (Range.constant 0 85399)
