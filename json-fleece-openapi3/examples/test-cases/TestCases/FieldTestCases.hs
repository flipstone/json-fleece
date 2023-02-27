{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases
  ( FieldTestCases(..)
  , fieldTestCasesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Maybe, Show)
import TestCases.FieldTestCases.ArrayFieldItem (ArrayFieldItem, arrayFieldItemSchema)
import TestCases.FieldTestCases.NullableArrayFieldItem (NullableArrayFieldItem, nullableArrayFieldItemSchema)
import TestCases.FieldTestCases.OptionalArrayFieldItem (OptionalArrayFieldItem, optionalArrayFieldItemSchema)
import TestCases.FieldTestCases.OptionalField (OptionalField, optionalFieldSchema)
import TestCases.FieldTestCases.OptionalNullableArrayFieldItem (OptionalNullableArrayFieldItem, optionalNullableArrayFieldItemSchema)
import TestCases.FieldTestCases.OptionalNullableField (OptionalNullableField, optionalNullableFieldSchema)
import TestCases.FieldTestCases.RequiredField (RequiredField, requiredFieldSchema)
import TestCases.FieldTestCases.RequiredNullableField (RequiredNullableField, requiredNullableFieldSchema)

data FieldTestCases = FieldTestCases
  { optionalNullableArrayField :: Maybe (Either FC.Null [OptionalNullableArrayFieldItem])
  , requiredNullableField :: Either FC.Null RequiredNullableField
  , arrayField :: [ArrayFieldItem]
  , optionalField :: Maybe OptionalField
  , optionalArrayField :: Maybe [OptionalArrayFieldItem]
  , nullableArrayField :: Either FC.Null [NullableArrayFieldItem]
  , requiredField :: RequiredField
  , optionalNullableField :: Maybe (Either FC.Null OptionalNullableField)
  }
  deriving (Eq, Show)

fieldTestCasesSchema :: FC.Fleece schema => schema FieldTestCases
fieldTestCasesSchema =
  FC.object $
    FC.constructor FieldTestCases
      #+ FC.optional "optionalNullableArrayField" optionalNullableArrayField (FC.nullable (FC.list optionalNullableArrayFieldItemSchema))
      #+ FC.required "requiredNullableField" requiredNullableField (FC.nullable requiredNullableFieldSchema)
      #+ FC.required "arrayField" arrayField (FC.list arrayFieldItemSchema)
      #+ FC.optional "optionalField" optionalField optionalFieldSchema
      #+ FC.optional "optionalArrayField" optionalArrayField (FC.list optionalArrayFieldItemSchema)
      #+ FC.required "nullableArrayField" nullableArrayField (FC.nullable (FC.list nullableArrayFieldItemSchema))
      #+ FC.required "requiredField" requiredField requiredFieldSchema
      #+ FC.optional "optionalNullableField" optionalNullableField (FC.nullable optionalNullableFieldSchema)