{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  , UnionExample
  , unionExampleSchema
  , TaggedUnionExample
  , taggedUnionExampleSchema
  , Person (..)
  , personObject
  , Company (..)
  , companyObject
  , SchemaValidatorInfo (..)
  , ValidatorInfo (..)
  , CustomValidatorObject (..)
  , customValidatorObjectExampleSchema
  , PositiveInt (..)
  , NegativeInt (..)
  , CustomValidator (..)
  , CustomValidatorInfo (..)
  ) where

import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Shrubbery (type (@=))
import qualified Shrubbery

import Fleece.Core
  ( AdditionalFields
  , Field
  , Fleece
  , FleeceValidator
  , Name
  , NothingEncoding (EmitNull, OmitKey)
  , Null
  , Object
  , TaggedUnionMembers
  , UnionMembers
  , Validator
  , additional
  , additionalFields
  , annotateName
  , array
  , bareOrJSONString
  , boolean
  , boundedEnum
  , boundedEnumNamed
  , compose
  , constructor
  , field
  , identity
  , int
  , jsonString
  , list
  , mapField
  , mkValidator
  , null
  , nullable
  , number
  , object
  , objectNamed
  , optional
  , optionalNullable
  , required
  , schemaName
  , taggedUnionCombine
  , taggedUnionMember
  , taggedUnionMemberWithTag
  , taggedUnionNamed
  , text
  , unboundedIntegralNumber
  , unionCombine
  , unionMember
  , unionMemberWithIndex
  , unionNamed
  , unqualifiedName
  , validate
  , validateNamed
  , (#*)
  , (#+)
  , (#@)
  , (#|)
  )
import qualified Fleece.Core as FC

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
    ( mkValidator
        (\(Validation t) -> t)
        (\t -> if T.length t > 12 then Left "At most 12 characters allowed" else Right (Validation t))
    )
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

type UnionExample =
  Shrubbery.Union [T.Text, Scientific]

unionExampleSchema :: Fleece schema => schema UnionExample
unionExampleSchema =
  unionNamed (unqualifiedName "UnionExample") $
    unionMember text
      #| unionMember number

type TaggedUnionExample =
  Shrubbery.TaggedUnion
    [ "person" @= Person
    , "company" @= Company
    ]

taggedUnionExampleSchema :: Fleece schema => schema TaggedUnionExample
taggedUnionExampleSchema =
  taggedUnionNamed (unqualifiedName "TaggedUnionExample") "type" $
    taggedUnionMember @"person" personObject
      #@ taggedUnionMember @"company" companyObject

data Person = Person
  { personName :: T.Text
  , personAge :: Int
  }
  deriving (Eq, Show)

personObject :: Fleece schema => Object schema Person Person
personObject =
  constructor Person
    #+ required "name" personName text
    #+ required "age" personAge int

data Company = Company
  { companyName :: T.Text
  , companyIsToBigToFail :: Bool
  }
  deriving (Eq, Show)

companyObject :: Fleece schema => Object schema Company Company
companyObject =
  constructor Company
    #+ required "name" companyName text
    #+ required "tooBigToFail" companyIsToBigToFail boolean

-- A custom class that extends Fleece validation.
class CustomValidator validator where
  integralMaximum :: Integral n => n -> validator n n
  integralMinimum :: Integral n => n -> validator n n

-- Implement the 'CustomValidator' class for the associated 'Validator' types of 'Fleece'
-- instances you want to use. The 'Validator' types in @json-fleece@ are either 'StandardValidator'
-- or 'NoOpValidator', so you can use packages like @json-fleece-aeson@ by implementing the
-- 'CustomValidator' class for them:
instance CustomValidator FC.StandardValidator where
  integralMaximum n =
    FC.mkValidator
      id
      (\x -> if x > n then Left "Too big" else Right x)
  integralMinimum n =
    FC.mkValidator
      id
      (\x -> if x < n then Left "Too small" else Right x)

instance CustomValidator FC.NoOpValidator where
  integralMaximum _ = FC.NoOpValidator
  integralMinimum _ = FC.NoOpValidator

-- To use your custom validation class in schemas, use a constraint on the 'Validator' type:
type CustomFleece schema =
  ( Fleece schema
  , CustomValidator (FC.Validator schema)
  )

boundedIntegral :: (Integral n, CustomFleece schema, Typeable n) => Maybe n -> Maybe n -> schema n
boundedIntegral mbMin mbMax =
  let
    validator =
      maybe identity integralMinimum mbMin
        `compose` maybe identity integralMaximum mbMax
  in
    validate
      validator
      unboundedIntegralNumber

newtype PositiveInt = PositiveInt Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

positiveIntSchema :: CustomFleece schema => schema PositiveInt
positiveIntSchema =
  boundedIntegral @PositiveInt (Just 0) Nothing

newtype NegativeInt = NegativeInt Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

negativeIntSchema :: CustomFleece schema => schema NegativeInt
negativeIntSchema =
  boundedIntegral @NegativeInt Nothing (Just (-1))

data CustomValidatorObject = CustomValidatorObject
  { customValidatorObjectPositiveInt :: PositiveInt
  , customValidatorObjectNegativeInt :: NegativeInt
  }
  deriving (Eq, Show)

customValidatorObjectExampleSchema :: CustomFleece schema => schema CustomValidatorObject
customValidatorObjectExampleSchema =
  object $
    constructor CustomValidatorObject
      #+ required "positive_int" customValidatorObjectPositiveInt positiveIntSchema
      #+ required "negative_int" customValidatorObjectNegativeInt negativeIntSchema

-- The extended validator allows us to extract static information about the validations being performed
-- by implementing a new 'Fleece' instance. This example instance collects information about the custom
-- validations used by the schema.
data SchemaValidatorInfo a = SchemaValidatorInfo
  { schemaValidatorInfoName :: Name
  , schemaValidatorInfo :: ValidatorInfo
  }
  deriving (Eq, Show)

primInfo :: String -> SchemaValidatorInfo a
primInfo name = SchemaValidatorInfo (unqualifiedName name) (ValidatorInfo [] [])

data ValidatorInfo = ValidatorInfo
  { validatorInfoCustomValidatorInfo :: [CustomValidatorInfo]
  , validatorInfoChildren :: [ValidatorInfo]
  }
  deriving (Eq, Show)

appendValidatorInfo :: ValidatorInfo -> CollectValidatorInfo a b -> ValidatorInfo
appendValidatorInfo (ValidatorInfo infos1 cs) (CollectValidatorInfo infos2) =
  ValidatorInfo (infos1 <> infos2) cs

-- This type will serve as our 'Validator' associated type
newtype CollectValidatorInfo a b = CollectValidatorInfo [CustomValidatorInfo]
  deriving (Semigroup, Monoid)

retag :: CollectValidatorInfo a b -> CollectValidatorInfo c d
retag (CollectValidatorInfo x) = CollectValidatorInfo x

instance FleeceValidator CollectValidatorInfo where
  mkValidator _ _ = CollectValidatorInfo mempty
  compose a b = retag a <> retag b

instance CustomValidator CollectValidatorInfo where
  integralMaximum n = CollectValidatorInfo [IntegralMaximum $ toInteger n]
  integralMinimum n = CollectValidatorInfo [IntegralMinimum $ toInteger n]

data CustomValidatorInfo
  = IntegralMinimum Integer
  | IntegralMaximum Integer
  deriving (Eq, Show)

instance Fleece SchemaValidatorInfo where
  newtype Object SchemaValidatorInfo _a _b = Object [ValidatorInfo]

  newtype Field SchemaValidatorInfo _a _b = Field ValidatorInfo

  newtype AdditionalFields SchemaValidatorInfo _a _b = AdditionalFields ValidatorInfo

  newtype UnionMembers SchemaValidatorInfo _a _b = UnionMembers [ValidatorInfo]

  newtype TaggedUnionMembers SchemaValidatorInfo _a _b = TaggedUnionMembers [ValidatorInfo]

  -- Derive the instance of 'CustomValidator' using @GeneralizedNewtypeDeriving@
  type Validator SchemaValidatorInfo = CollectValidatorInfo

  schemaName (SchemaValidatorInfo n _) = n

  number = primInfo "scientific"

  text = primInfo "text"

  boolean = primInfo "boolean"

  array (SchemaValidatorInfo a b) = SchemaValidatorInfo (a `annotateName` "array") b

  null = primInfo "null"

  nullable (SchemaValidatorInfo a b) = SchemaValidatorInfo (a `annotateName` "nullable") b

  required _ _ (SchemaValidatorInfo _ info) = Field info

  optional _ _ (SchemaValidatorInfo _ info) = Field info

  mapField _ (Field f) =
    Field f

  additionalFields _ (SchemaValidatorInfo _ info) =
    AdditionalFields info

  objectNamed n (Object infos) =
    SchemaValidatorInfo n (ValidatorInfo [] infos)

  constructor _ =
    Object mempty

  field (Object objInfos) (Field fieldInfo) =
    Object (fieldInfo : objInfos)

  additional (Object objInfos) (AdditionalFields fieldsInfo) =
    Object (fieldsInfo : objInfos)

  validateNamed n collectValidatorInfo (SchemaValidatorInfo _ info) =
    SchemaValidatorInfo n (appendValidatorInfo info collectValidatorInfo)

  boundedEnumNamed n _ =
    SchemaValidatorInfo n (ValidatorInfo [] [])

  unionNamed n (UnionMembers infos) =
    SchemaValidatorInfo n (ValidatorInfo [] infos)

  unionMemberWithIndex _ (SchemaValidatorInfo _ info) =
    UnionMembers [info]

  unionCombine (UnionMembers infos1) (UnionMembers infos2) =
    UnionMembers (infos1 <> infos2)

  taggedUnionNamed n _ (TaggedUnionMembers infos) =
    SchemaValidatorInfo n (ValidatorInfo [] infos)

  taggedUnionMemberWithTag _ (Object info) =
    TaggedUnionMembers info

  taggedUnionCombine (TaggedUnionMembers info1) (TaggedUnionMembers info2) =
    TaggedUnionMembers (info1 <> info2)

  jsonString s = s
