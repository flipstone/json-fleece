{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Text.Lazy as LT
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import qualified Fleece.Examples as Examples
import qualified Fleece.Markdown as FM

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-markdown" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_object", prop_object)
  , ("prop_boundedEnum", prop_boundedEnum)
  , ("prop_nullableField", prop_nullableField)
  , ("prop_validate", prop_validate)
  , ("prop_optionalField_EmitNull_AcceptNull", prop_optionalField_EmitNull_AcceptNull)
  , ("prop_optionalField_OmitKey_AcceptNull", prop_optionalField_OmitKey_AcceptNull)
  , ("prop_optionalField_OmitKey_DelegateNull", prop_optionalField_OmitKey_DelegateNull)
  , ("prop_optionalField_OmitKey_DelegateNull_Nullable", prop_optionalField_OmitKey_DelegateNull_Nullable)
  ]

prop_object :: HH.Property
prop_object =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.fooBarSchema
      === LT.intercalate
        "\n"
        [ "# FooBar"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|foo|yes|no|string|"
        , "|bar|yes|no|number|"
        , ""
        ]

prop_boundedEnum :: HH.Property
prop_boundedEnum =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.boundedEnumExampleSchema
      === LT.intercalate
        "\n"
        [ "# BoundedEnumExample"
        , ""
        , "Enum values:"
        , ""
        , "- apple"
        , "- orange"
        , "- kumquat"
        , ""
        ]

prop_nullableField :: HH.Property
prop_nullableField =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.nullableFieldExampleSchema
      === LT.intercalate
        "\n"
        [ "# NullableFieldExample"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|nullableField|yes|yes|string|"
        , ""
        ]

prop_validate :: HH.Property
prop_validate =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.validationExampleSchema
      === LT.intercalate
        "\n"
        [ "# ValidationExample"
        , ""
        , "string (with validation restrictions)"
        , ""
        ]

prop_optionalField_EmitNull_AcceptNull :: HH.Property
prop_optionalField_EmitNull_AcceptNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_EmitNull_AcceptNull_ExampleSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_EmitNull_AcceptNull_Example"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_EmitNull_AcceptNull_Field|no|yes|string|"
        , ""
        ]

prop_optionalField_OmitKey_AcceptNull :: HH.Property
prop_optionalField_OmitKey_AcceptNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_AcceptNull_ExampleSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_OmitKey_AcceptNull_Example"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_OmitKey_AcceptNull_Field|no|yes|string|"
        , ""
        ]

prop_optionalField_OmitKey_DelegateNull :: HH.Property
prop_optionalField_OmitKey_DelegateNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_DelegateNull_ExampleSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_OmitKey_DelegateNull_Example"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_OmitKey_DelegateNull_Field|no|no|string|"
        , ""
        ]

prop_optionalField_OmitKey_DelegateNull_Nullable :: HH.Property
prop_optionalField_OmitKey_DelegateNull_Nullable =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_DelegateNull_NullableExampleSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_OmitKey_DelegateNull_NullableExample"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_OmitKey_DelegateNull_Nullable_Field|no|yes|string|"
        , ""
        ]
