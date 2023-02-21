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
  , ("prop_embeddedObject", prop_embeddedObject)
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
    FM.renderMarkdown Examples.boundedEnumSchema
      === LT.intercalate
        "\n"
        [ "# BoundedEnum"
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
    FM.renderMarkdown Examples.nullableFieldSchema
      === LT.intercalate
        "\n"
        [ "# NullableField"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|nullableField|yes|yes|string|"
        , ""
        ]

prop_validate :: HH.Property
prop_validate =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.validationSchema
      === LT.intercalate
        "\n"
        [ "# Validation"
        , ""
        , "string (with validation restrictions)"
        , ""
        ]

prop_optionalField_EmitNull_AcceptNull :: HH.Property
prop_optionalField_EmitNull_AcceptNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_EmitNull_AcceptNullSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_EmitNull_AcceptNull"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_EmitNull_AcceptNull_Field|no|yes|string|"
        , ""
        ]

prop_optionalField_OmitKey_AcceptNull :: HH.Property
prop_optionalField_OmitKey_AcceptNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_AcceptNullSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_OmitKey_AcceptNull"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_OmitKey_AcceptNull_Field|no|yes|string|"
        , ""
        ]

prop_optionalField_OmitKey_DelegateNull :: HH.Property
prop_optionalField_OmitKey_DelegateNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_DelegateNullSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_OmitKey_DelegateNull"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_OmitKey_DelegateNull_Field|no|no|string|"
        , ""
        ]

prop_optionalField_OmitKey_DelegateNull_Nullable :: HH.Property
prop_optionalField_OmitKey_DelegateNull_Nullable =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_DelegateNull_NullableSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField_OmitKey_DelegateNull_Nullable"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional_OmitKey_DelegateNull_Nullable_Field|no|yes|string|"
        , ""
        ]

prop_embeddedObject :: HH.Property
prop_embeddedObject =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.embeddedObjectParentSchema
      === LT.intercalate
        "\n"
        [ "# EmbeddedObjectParent"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|parentField|yes|no|string|"
        , "|childField|yes|no|string|"
        , ""
        ]
