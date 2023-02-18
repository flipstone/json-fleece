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
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Fleece.Examples (FooBar (FooBar, bar, foo), fooBarSchema)

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-aeson" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_decode_NumberSuccess", prop_decode_NumberSuccess)
  , ("prop_encodeNumber", prop_encodeNumber)
  , ("prop_decode_TextSuccess", prop_decode_TextSuccess)
  , ("prop_encodeText", prop_encodeText)
  , ("prop_decode_ObjectSuccess", prop_decode_ObjectSuccess)
  , ("prop_encodeObject", prop_encodeObject)
  , ("prop_decode_optionalField_EmitNull_AcceptNull", prop_decode_optionalField_EmitNull_AcceptNull)
  , ("prop_encode_optionalField_EmitNull_AcceptNull", prop_encode_optionalField_EmitNull_AcceptNull)
  , ("prop_decode_optionalField_OmitKey_AcceptNull", prop_decode_optionalField_OmitKey_AcceptNull)
  , ("prop_encode_optionalField_OmitKey_AcceptNull", prop_encode_optionalField_OmitKey_AcceptNull)
  , ("prop_decode_optionalField_OmitKey_DelegateNull", prop_decode_optionalField_OmitKey_DelegateNull)
  , ("prop_encode_optionalField_OmitKey_DelegateNull", prop_encode_optionalField_OmitKey_DelegateNull)
  , ("prop_decode_optionalField_OmitKey_DelegateNull_Failure", prop_decode_optionalField_OmitKey_DelegateNull_Failure)
  ]

prop_decode_NumberSuccess :: HH.Property
prop_decode_NumberSuccess =
  HH.property $ do
    num <- HH.forAll genScientific
    FA.decode FC.number (Aeson.encode (Aeson.Number num)) === Right num

prop_encodeNumber :: HH.Property
prop_encodeNumber =
  HH.property $ do
    num <- HH.forAll genScientific
    FA.encode FC.number num === Aeson.encode (Aeson.Number num)

prop_decode_TextSuccess :: HH.Property
prop_decode_TextSuccess =
  HH.property $ do
    text <- HH.forAll genText
    FA.decode FC.text (Aeson.encode (Aeson.String text)) === Right text

prop_encodeText :: HH.Property
prop_encodeText =
  HH.property $ do
    text <- HH.forAll genText
    FA.encode FC.text text === Aeson.encode (Aeson.String text)

prop_decode_ObjectSuccess :: HH.Property
prop_decode_ObjectSuccess =
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
        FooBar
          { foo = fooValue
          , bar = barValue
          }

    FA.decode fooBarSchema jsonObject === Right expected

prop_encodeObject :: HH.Property
prop_encodeObject =
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
        FooBar
          { foo = fooValue
          , bar = barValue
          }

    FA.encode fooBarSchema fooBar === expected

prop_encode_optionalField_EmitNull_AcceptNull :: HH.Property
prop_encode_optionalField_EmitNull_AcceptNull =
  HH.property $ do
    let
      schema :: FC.Fleece schema => schema (Maybe T.Text)
      schema =
        FC.object $
          FC.constructor id
            #+ FC.optionalField FC.EmitNull_AcceptNull "value" id FC.text

    mbText <- HH.forAll (Gen.maybe genText)
    FA.encode schema mbText === encodeTestObject ["value" .= mbText]

prop_decode_optionalField_EmitNull_AcceptNull :: HH.Property
prop_decode_optionalField_EmitNull_AcceptNull =
  HH.property $ do
    let
      schema :: FC.Fleece schema => schema (Maybe T.Text)
      schema =
        FC.object $
          FC.constructor id
            #+ FC.optionalField FC.EmitNull_AcceptNull "value" id FC.text

    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      testInput =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["value" .= mbText]
            Nothing -> []

    FA.decode schema testInput === Right (join mbMbText)

prop_encode_optionalField_OmitKey_AcceptNull :: HH.Property
prop_encode_optionalField_OmitKey_AcceptNull =
  HH.property $ do
    let
      schema :: FC.Fleece schema => schema (Maybe T.Text)
      schema =
        FC.object $
          FC.constructor id
            #+ FC.optionalField FC.OmitKey_AcceptNull "value" id FC.text

    mbText <- HH.forAll (Gen.maybe genText)

    let
      expected =
        encodeTestObject $
          case mbText of
            Just text -> ["value" .= text]
            Nothing -> []

    FA.encode schema mbText === expected

prop_decode_optionalField_OmitKey_AcceptNull :: HH.Property
prop_decode_optionalField_OmitKey_AcceptNull =
  HH.property $ do
    let
      schema :: FC.Fleece schema => schema (Maybe T.Text)
      schema =
        FC.object $
          FC.constructor id
            #+ FC.optionalField FC.OmitKey_AcceptNull "value" id FC.text

    mbMbText <- HH.forAll (Gen.maybe (Gen.maybe genText))

    let
      testInput =
        encodeTestObject $
          case mbMbText of
            Just mbText -> ["value" .= mbText]
            Nothing -> []

    FA.decode schema testInput === Right (join mbMbText)

prop_encode_optionalField_OmitKey_DelegateNull :: HH.Property
prop_encode_optionalField_OmitKey_DelegateNull =
  HH.property $ do
    let
      schema :: FC.Fleece schema => schema (Maybe T.Text)
      schema =
        FC.object $
          FC.constructor id
            #+ FC.optionalField FC.OmitKey_DelegateNull "value" id FC.text

    mbText <- HH.forAll (Gen.maybe genText)

    let
      expected =
        encodeTestObject $
          case mbText of
            Just text -> ["value" .= text]
            Nothing -> []

    FA.encode schema mbText === expected

prop_decode_optionalField_OmitKey_DelegateNull :: HH.Property
prop_decode_optionalField_OmitKey_DelegateNull =
  HH.property $ do
    let
      schema :: FC.Fleece schema => schema (Maybe T.Text)
      schema =
        FC.object $
          FC.constructor id
            #+ FC.optionalField FC.OmitKey_DelegateNull "value" id FC.text

    mbText <- HH.forAll (Gen.maybe genText)

    let
      testInput =
        encodeTestObject $
          case mbText of
            Just text -> ["value" .= text]
            Nothing -> []

    FA.decode schema testInput === Right mbText

prop_decode_optionalField_OmitKey_DelegateNull_Failure :: HH.Property
prop_decode_optionalField_OmitKey_DelegateNull_Failure =
  HH.withTests 1 . HH.property $ do
    let
      schema :: FC.Fleece schema => schema (Maybe T.Text)
      schema =
        FC.object $
          FC.constructor id
            #+ FC.optionalField FC.OmitKey_DelegateNull "value" id FC.text

      testInput =
        encodeTestObject
          [ "value" .= (Nothing :: Maybe T.Text)
          ]

    FA.decode schema testInput
      === Left "Error in $.value: parsing text failed, expected String, but encountered Null"

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
