{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases
  ( FieldTestCases(..)
  , fieldTestCasesSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Maybe, Show)
import qualified TestCases.Types.FieldTestCases.ArrayFieldItem as ArrayFieldItem
import qualified TestCases.Types.FieldTestCases.NullableArrayFieldItem as NullableArrayFieldItem
import qualified TestCases.Types.FieldTestCases.OptionalArrayFieldItem as OptionalArrayFieldItem
import qualified TestCases.Types.FieldTestCases.OptionalField as OptionalField
import qualified TestCases.Types.FieldTestCases.OptionalNullableArrayFieldItem as OptionalNullableArrayFieldItem
import qualified TestCases.Types.FieldTestCases.OptionalNullableField as OptionalNullableField
import qualified TestCases.Types.FieldTestCases.RequiredField as RequiredField
import qualified TestCases.Types.FieldTestCases.RequiredNullableField as RequiredNullableField

data FieldTestCases = FieldTestCases
  { arrayField :: [ArrayFieldItem.ArrayFieldItem]
  , nullableArrayField :: Either FC.Null [NullableArrayFieldItem.NullableArrayFieldItem]
  , optionalArrayField :: Maybe [OptionalArrayFieldItem.OptionalArrayFieldItem]
  , optionalField :: Maybe OptionalField.OptionalField
  , optionalNullableArrayField :: Maybe (Either FC.Null [OptionalNullableArrayFieldItem.OptionalNullableArrayFieldItem])
  , optionalNullableField :: Maybe (Either FC.Null OptionalNullableField.OptionalNullableField)
  , requiredField :: RequiredField.RequiredField
  , requiredNullableField :: Either FC.Null RequiredNullableField.RequiredNullableField
  }
  deriving (Eq, Show)

fieldTestCasesSchema :: FC.Fleece schema => schema FieldTestCases
fieldTestCasesSchema =
  FC.object $
    FC.constructor FieldTestCases
      #+ FC.required "arrayField" arrayField (FC.list ArrayFieldItem.arrayFieldItemSchema)
      #+ FC.required "nullableArrayField" nullableArrayField (FC.nullable (FC.list NullableArrayFieldItem.nullableArrayFieldItemSchema))
      #+ FC.optional "optionalArrayField" optionalArrayField (FC.list OptionalArrayFieldItem.optionalArrayFieldItemSchema)
      #+ FC.optional "optionalField" optionalField OptionalField.optionalFieldSchema
      #+ FC.optional "optionalNullableArrayField" optionalNullableArrayField (FC.nullable (FC.list OptionalNullableArrayFieldItem.optionalNullableArrayFieldItemSchema))
      #+ FC.optional "optionalNullableField" optionalNullableField (FC.nullable OptionalNullableField.optionalNullableFieldSchema)
      #+ FC.required "requiredField" requiredField RequiredField.requiredFieldSchema
      #+ FC.required "requiredNullableField" requiredNullableField (FC.nullable RequiredNullableField.requiredNullableFieldSchema)