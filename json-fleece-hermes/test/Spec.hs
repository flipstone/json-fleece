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
import qualified Data.ByteString as BS
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

import qualified Fleece.Core as FC
import qualified Fleece.Examples as Examples
import qualified Fleece.Hermes as FH

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-hermes - json" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_decode_number", prop_decode_number)
  , ("prop_decode_string", prop_decode_string)
  , ("prop_decode_boolean", prop_decode_boolean)
  , ("prop_decode_null", prop_decode_null)
  , ("prop_decode_array", prop_decode_array)
  , ("prop_decode_object", prop_decode_object)
  , ("prop_decode_boundedEnum", prop_decode_boundedEnum)
  , ("prop_decode_nullableField", prop_decode_nullableField)
  , ("prop_decode_validate", prop_decode_validate)
  , ("prop_decode_nullableField_Failure", prop_decode_nullableField_Failure)
  , ("prop_decode_optionalNullableFieldEmitNull", prop_decode_optionalNullableFieldEmitNull)
  , ("prop_decode_optionalNullableFieldOmitKey", prop_decode_optionalNullableFieldOmitKey)
  , ("prop_decode_additional", prop_decode_additional)
  , ("prop_decode_abnormalNumbers", prop_decode_abnormalNumbers)
  , ("prop_decode_listField", prop_decode_listField)
  , ("prop_utcTimeAndZonedTime", prop_utcTimeAndZonedTime)
  ]

prop_decode_number :: HH.Property
prop_decode_number =
  HH.property $ do
    num <- HH.forAll genScientific
    let
      encoded = encodeTestObject ["foo" .= Aeson.Number num]
    FH.decode (dummyObj "foo" FC.number) encoded === Right num

prop_decode_string :: HH.Property
prop_decode_string =
  HH.property $ do
    text <- HH.forAll genText
    let
      encoded = encodeTestObject ["foo" .= Aeson.String text]
    FH.decode (dummyObj "foo" FC.text) encoded === Right text

prop_decode_boolean :: HH.Property
prop_decode_boolean =
  HH.property $ do
    bool <- HH.forAll Gen.bool
    let
      encoded = encodeTestObject ["foo" .= Aeson.Bool bool]
    FH.decode (dummyObj "foo" FC.boolean) encoded === Right bool

prop_decode_null :: HH.Property
prop_decode_null =
  HH.withTests 1 . HH.property $ do
    let
      encoded = encodeTestObject ["foo" .= Aeson.Null]
    FH.decode (dummyObj "foo" FC.null) encoded === Right FC.Null

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
        LBS.toStrict
          . Aeson.encode
          . Aeson.Array
          . fmap Aeson.toJSON
          $ texts

      expected =
        Right texts

      decoded =
        FH.decode
          (FC.array FC.text)
          testInput

    decoded === expected

prop_decode_object :: HH.Property
prop_decode_object =
  HH.property $ do
    fooValue <- HH.forAll genText
    barValue <- HH.forAll genScientific

    let
      jsonObject =
        encodeTestObject
          [ "foo" .= Aeson.String fooValue
          , "bar" .= Aeson.Number barValue
          ]

      expected =
        Examples.FooBar
          { Examples.foo = fooValue
          , Examples.bar = barValue
          }

    FH.decode Examples.fooBarSchema jsonObject === Right expected

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
        encodeTestObject
          [ "foo" .= Aeson.String textValue
          ]

      expected =
        case textValue of
          "apple" -> Right Examples.Apple
          "orange" -> Right Examples.Orange
          "kumquat" -> Right Examples.Kumquat
          _ ->
            Left $
              "Error in /foo: Unrecognized value for Fleece.Examples.BoundedEnum enum: "
                <> show textValue

      decoded =
        FH.decode
          (dummyObj "foo" Examples.boundedEnumSchema)
          testInput

    decoded === expected

prop_decode_nullableField :: HH.Property
prop_decode_nullableField =
  HH.property $ do
    nullOrText <- HH.forAll (Gen.either (pure FC.Null) genText)

    let
      testInput =
        encodeTestObject
          ["nullableField" .= either (const Aeson.Null) Aeson.String nullOrText]

      decoded =
        FH.decode
          Examples.nullableFieldSchema
          testInput

      expected =
        Right
          . Examples.NullableField
          $ nullOrText

    decoded === expected

prop_decode_validate :: HH.Property
prop_decode_validate =
  HH.property $ do
    text <- HH.forAll genText

    let
      testInput =
        encodeTestObject
          [ "foo" .= Aeson.String text
          ]

      decoded =
        FH.decode (dummyObj "foo" Examples.validationSchema) testInput

      expected =
        if T.length text > 12
          then
            Left
              "Error in /foo: Error validating Fleece.Examples.Validation: \
              \At most 12 characters allowed"
          else Right (Examples.Validation text)

    decoded === expected

prop_decode_nullableField_Failure :: HH.Property
prop_decode_nullableField_Failure =
  HH.withTests 1 . HH.property $ do
    let
      testInput =
        encodeTestObject []

      decoded =
        FH.decode
          Examples.nullableFieldSchema
          testInput

      expected =
        Left
          "Error in /nullableField: NO_SUCH_FIELD: The JSON field referenced does not \
          \exist in this object."

    decoded === expected

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
        FH.decode
          Examples.optionalNullableFieldEmitNullSchema
          testInput

      expected =
        Right
          . Examples.OptionalNullableFieldEmitNull
          . join
          $ mbMbText

    decoded === expected

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
        FH.decode
          Examples.optionalNullableFieldOmitKeySchema
          testInput

      expected =
        Right
          . Examples.OptionalNullableFieldOmitKey
          . join
          $ mbMbText

    decoded === expected

prop_decode_additional :: HH.Property
prop_decode_additional =
  HH.property $ do
    field1 <- HH.forAll genText
    field2 <- HH.forAll genText

    let
      genPair = do
        name <- Gen.filter (\e -> e `notElem` ["field1", "field2"]) genText
        value <- genText
        pure (name, value)

    others <- HH.forAll (Gen.map (Range.linear 0 10) genPair)

    let
      testInput =
        encodeTestObject
          ( "field1" .= field1
              : "field2" .= field2
              : map (\(k, v) -> AesonKey.fromText k .= v) (Map.toList others)
          )

      decoded =
        FH.decode
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
        FH.decode Examples.abnormalNumbersExampleSchema encoded

      expected =
        Examples.AbnormalNumbersExample
          { Examples.stringyNumber = stringyNumber
          , Examples.bareOrStringyNumber = bareOrStringyNumber
          }

    Right expected === decoded

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

    FH.decode Examples.listFieldExampleSchema jsonObject === Right expected

prop_utcTimeAndZonedTime :: HH.Property
prop_utcTimeAndZonedTime =
  HH.property $ do
    utcOrZonedTime <-
      HH.forAll $ Gen.either genUTCTime genZonedTime

    let
      encoded =
        case utcOrZonedTime of
          Left utcTime ->
            encodeTestObject
              [ "time" .= utcTime
              ]
          Right zonedTime ->
            encodeTestObject
              [ "time" .= zonedTime
              ]

      -- we only compare the minutes offset of the time zone because
      -- otherwise UTC and +0000 are not equal
      zonedTimeToTuple zonedTime =
        ( Time.zonedTimeToLocalTime zonedTime
        , Time.timeZoneMinutes (Time.zonedTimeZone zonedTime)
        )

    utcOrZonedTimeDecoded <-
      HH.forAll $
        Gen.either
          (pure $ FH.decode (dummyObj "time" FC.utcTime) encoded)
          (pure $ FH.decode (dummyObj "time" FC.zonedTime) encoded)

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

encodeTestObject :: [AesonEncoding.Series] -> BS.ByteString
encodeTestObject =
  LBS.toStrict
    . AesonEncoding.encodingToLazyByteString
    . Aeson.pairs
    . mconcat

encodeAesonText :: Aeson.ToJSON a => a -> T.Text
encodeAesonText =
  LT.toStrict . AesonText.encodeToLazyText

dummyObj :: FC.Fleece schema => String -> schema a -> schema a
dummyObj field schema =
  FC.objectNamed "" $
    FC.constructor id
      FC.#+ FC.required field id schema

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
