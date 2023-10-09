module Fleece.Examples
  ( FooBar (..)
  , fooBarSchema
  , NullableField (..)
  , nullableFieldSchema
  , Validation (..)
  , validationSchema
  , OptionalField (..)
  , optionalFieldSchema
  , OptionalNullableFieldEmitNull (..)
  , optionalNullableFieldEmitNullSchema
  , OptionalNullableFieldOmitKey (..)
  , optionalNullableFieldOmitKeySchema
  , BoundedEnum (..)
  , boundedEnumSchema
  , boundedEnumToText
  , AdditionalFieldsExample (..)
  , additionalFieldsExampleSchema
  , AbnormalNumbersExample (..)
  , abnormalNumbersExampleSchema
  , ListFieldExample (..)
  , listFieldExampleSchema
  , CrossFieldValidationExample (..)
  , mkCrossFieldValidationExample
  , crossFieldValidationExampleSchema
  , InlineObjectExample (..)
  , inlineObjectExampleSchema
  ) where

import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T

import Fleece.Core
  ( Fleece
  , InlineObject (InlineObject)
  , NothingEncoding (EmitNull, OmitKey)
  , Null
  , Object
  , additionalFields
  , bareOrJSONString
  , boundedEnum
  , constructor
  , int
  , jsonString
  , list
  , nullable
  , number
  , object
  , optional
  , optionalNullable
  , required
  , text
  , validate
  , validateObject
  , (#*)
  , (#+)
  , (#:)
  )

data FooBar = FooBar
  { foo :: T.Text
  , bar :: Scientific
  }
  deriving (Eq, Show)

fooBarSchema :: Fleece schema => schema FooBar
fooBarSchema =
  object $
    constructor FooBar
      #+ required "foo" foo text
      #+ required "bar" bar number

data NullableField = NullableField
  { exampleNullableField :: Either Null T.Text
  }
  deriving (Eq, Show)

nullableFieldSchema :: Fleece schema => schema NullableField
nullableFieldSchema =
  object $
    constructor NullableField
      #+ required "nullableField" exampleNullableField (nullable text)

newtype Validation = Validation T.Text
  deriving (Eq, Show)

validationSchema :: Fleece schema => schema Validation
validationSchema =
  validate
    (\(Validation t) -> t)
    (\t -> if T.length t > 12 then Left "At most 12 characters allowed" else Right (Validation t))
    text

data OptionalField = OptionalField
  { exampleOptionalField :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalFieldSchema :: Fleece schema => schema OptionalField
optionalFieldSchema =
  object $
    constructor OptionalField
      #+ optional "optionalField" exampleOptionalField text

data OptionalNullableFieldEmitNull = OptionalNullableFieldEmitNull
  { exampleOptionalNullableFieldEmitNullField :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalNullableFieldEmitNullSchema ::
  Fleece schema =>
  schema OptionalNullableFieldEmitNull
optionalNullableFieldEmitNullSchema =
  object $
    constructor OptionalNullableFieldEmitNull
      #+ optionalNullable EmitNull "optionalNullableField" exampleOptionalNullableFieldEmitNullField text

data OptionalNullableFieldOmitKey = OptionalNullableFieldOmitKey
  { exampleOptionalNullableFieldOmitKeyField :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalNullableFieldOmitKeySchema ::
  Fleece schema =>
  schema OptionalNullableFieldOmitKey
optionalNullableFieldOmitKeySchema =
  object $
    constructor OptionalNullableFieldOmitKey
      #+ optionalNullable OmitKey "optionalNullableField" exampleOptionalNullableFieldOmitKeyField text

data BoundedEnum
  = Apple
  | Orange
  | Kumquat
  deriving (Eq, Show, Enum, Bounded)

boundedEnumSchema :: Fleece schema => schema BoundedEnum
boundedEnumSchema =
  boundedEnum boundedEnumToText

boundedEnumToText :: BoundedEnum -> T.Text
boundedEnumToText e =
  case e of
    Apple -> T.pack "apple"
    Orange -> T.pack "orange"
    Kumquat -> T.pack "kumquat"

data AdditionalFieldsExample = AdditionalFieldsExample
  { field1 :: T.Text
  , field2 :: T.Text
  , otherFields :: Map.Map T.Text T.Text
  }
  deriving (Eq, Show)

additionalFieldsExampleSchema :: Fleece schema => schema AdditionalFieldsExample
additionalFieldsExampleSchema =
  object $
    constructor AdditionalFieldsExample
      #+ required "field1" field1 text
      #+ required "field2" field2 text
      #* additionalFields otherFields text

data AbnormalNumbersExample = AbnormalNumbersExample
  { stringyNumber :: Scientific
  , bareOrStringyNumber :: Scientific
  }
  deriving (Eq, Show)

abnormalNumbersExampleSchema :: Fleece schema => schema AbnormalNumbersExample
abnormalNumbersExampleSchema =
  object $
    constructor AbnormalNumbersExample
      #+ required "stringyNumber" stringyNumber (jsonString number)
      #+ required "bareOrStringyNumber" bareOrStringyNumber (bareOrJSONString number)

newtype ListFieldExample = ListFieldExample
  { listField :: [BoundedEnum]
  }
  deriving (Eq, Show)

listFieldExampleSchema :: Fleece schema => schema ListFieldExample
listFieldExampleSchema =
  object $
    constructor ListFieldExample
      #+ required "listField" listField (list boundedEnumSchema)

data CrossFieldValidationExample = CrossFieldValidationExample
  { validationLengthField :: Int
  , validationTextField :: T.Text
  }

{- |
  A validating constructor for a 'CrossFieldValidationExample' that is used in
  'crossFieldValidationExampleSchema'.
-}
mkCrossFieldValidationExample ::
  Int ->
  T.Text ->
  Either String CrossFieldValidationExample
mkCrossFieldValidationExample len txt =
  if T.length txt == len
    then Right (CrossFieldValidationExample len txt)
    else Left "'length' field must be equal to the length of the 'text' field"

crossFieldValidationExampleSchema :: Fleece schema => schema CrossFieldValidationExample
crossFieldValidationExampleSchema =
  validateObject $
    constructor mkCrossFieldValidationExample
      #+ required "length" validationLengthField int
      #+ required "text" validationTextField text

data InlineObjectExample = InlineObjectExample
  { inlineObjectNormalField :: T.Text
  , inlineObjectSubObject :: InlineObjectSubObject
  }

data InlineObjectSubObject = InlineObjectSubObject
  { subObjectField1 :: T.Text
  , subObjectField2 :: T.Text
  }

inlineObjectExampleSchema :: Fleece schema => schema InlineObjectExample
inlineObjectExampleSchema =
  object $
    constructor InlineObjectExample
      #+ required "normalField" inlineObjectNormalField text
      #: InlineObject inlineObjectSubObject subObjectSchema

subObjectSchema :: Fleece schema => Object schema InlineObjectSubObject InlineObjectSubObject
subObjectSchema =
  constructor InlineObjectSubObject
    #+ required "field1" subObjectField1 text
    #+ required "field2" subObjectField2 text
