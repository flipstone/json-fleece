{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
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
  , ("prop_nestedObject", prop_nestedObject)
  , ("prop_nameDisambiguation", prop_nameDisambiguation)
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
        [ "# string"
        , ""
        , "string"
        ]

prop_optionalField_EmitNull_AcceptNull :: HH.Property
prop_optionalField_EmitNull_AcceptNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_EmitNull_AcceptNullSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField\\_EmitNull\\_AcceptNull"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional\\_EmitNull\\_AcceptNull\\_Field|no|yes|string|"
        , ""
        ]

prop_optionalField_OmitKey_AcceptNull :: HH.Property
prop_optionalField_OmitKey_AcceptNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_AcceptNullSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField\\_OmitKey\\_AcceptNull"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional\\_OmitKey\\_AcceptNull\\_Field|no|yes|string|"
        , ""
        ]

prop_optionalField_OmitKey_DelegateNull :: HH.Property
prop_optionalField_OmitKey_DelegateNull =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_DelegateNullSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField\\_OmitKey\\_DelegateNull"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional\\_OmitKey\\_DelegateNull\\_Field|no|no|string|"
        , ""
        ]

prop_optionalField_OmitKey_DelegateNull_Nullable :: HH.Property
prop_optionalField_OmitKey_DelegateNull_Nullable =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown Examples.optionalField_OmitKey_DelegateNull_NullableSchema
      === LT.intercalate
        "\n"
        [ "# OptionalField\\_OmitKey\\_DelegateNull\\_Nullable"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|optional\\_OmitKey\\_DelegateNull\\_Nullable\\_Field|no|yes|string|"
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

prop_nestedObject :: HH.Property
prop_nestedObject =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown parentSchema
      === LT.intercalate
        "\n"
        [ "# Parent"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|field1|yes|no|string|"
        , "|field2|yes|no|string|"
        , "|nested|yes|no|NestedObject|"
        , ""
        , "# NestedObject"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|field1|yes|no|string|"
        , "|field2|yes|no|string|"
        , ""
        ]

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

prop_nameDisambiguation :: HH.Property
prop_nameDisambiguation =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown ambiguousNameParentSchema
      === LT.intercalate
        "\n"
        [ "# AmbiguousNameParent"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|child1|yes|no|Child1.AmbiguousName|"
        , "|child2|yes|no|Child2.AmbiguousName|"
        , ""
        , "# Child1.AmbiguousName"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|value|yes|no|string|"
        , ""
        , "# Child2.AmbiguousName"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|value|yes|no|string|"
        , ""
        ]

data AmbiguousNameParent = AmbiguousNameParent
  { ambiguousNameChild1 :: AmbiguousNameChild1
  , ambiguousNameChild2 :: AmbiguousNameChild2
  }

ambiguousNameParentSchema :: FC.Fleece schema => schema AmbiguousNameParent
ambiguousNameParentSchema =
  FC.objectNamed "Parent.AmbiguousNameParent" $
    FC.constructor AmbiguousNameParent
      #+ FC.required "child1" ambiguousNameChild1 ambiguousNameChild1Schema
      #+ FC.required "child2" ambiguousNameChild2 ambiguousNameChild2Schema

newtype AmbiguousNameChild1 = AmbiguousNameChild1
  { ambiguousNameChild1Value :: T.Text
  }

ambiguousNameChild1Schema :: FC.Fleece schema => schema AmbiguousNameChild1
ambiguousNameChild1Schema =
  FC.objectNamed "Child1.AmbiguousName" $
    FC.constructor AmbiguousNameChild1
      #+ FC.required "value" ambiguousNameChild1Value FC.text

newtype AmbiguousNameChild2 = AmbiguousNameChild2
  { ambiguousNameChild2Value :: T.Text
  }

ambiguousNameChild2Schema :: FC.Fleece schema => schema AmbiguousNameChild2
ambiguousNameChild2Schema =
  FC.objectNamed "Child2.AmbiguousName" $
    FC.constructor AmbiguousNameChild2
      #+ FC.required "value" ambiguousNameChild2Value FC.text
