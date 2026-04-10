{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | Example Fleece schema definitions demonstrating various features. Useful as
a reference for how to define schemas for different JSON structures.
-}
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
  ) where

import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Shrubbery (type (@=))
import qualified Shrubbery

import Fleece.Core
  ( Fleece
  , NothingEncoding (EmitNull, OmitKey)
  , Null
  , Object
  , Schema
  , additionalFields
  , boolean
  , boundedEnum
  , constructor
  , describe
  , int
  , jsonString
  , list
  , nullable
  , number
  , object
  , optional
  , optionalNullable
  , required
  , taggedUnionMember
  , taggedUnionNamed
  , text
  , unionMember
  , unionNamed
  , unqualifiedName
  , validate
  , (#*)
  , (#+)
  , (#@)
  , (#|)
  )

-- | An example data type with a text field and a number field.
data FooBar = FooBar
  { foo :: T.Text
  -- ^ A text field.
  , bar :: Scientific
  -- ^ A number field.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'FooBar' demonstrating basic object and required field usage.
fooBarSchema :: Fleece t => Schema t FooBar
fooBarSchema =
  object $
    constructor FooBar
      #+ required "foo" foo text
      #+ required "bar" bar number

-- | An example data type with a nullable text field.
data NullableField = NullableField
  { exampleNullableField :: Either Null T.Text
  -- ^ A nullable text field.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'NullableField' demonstrating nullable field usage.
nullableFieldSchema :: Fleece t => Schema t NullableField
nullableFieldSchema =
  object $
    constructor NullableField
      #+ required "nullableField" exampleNullableField (nullable text)

-- | An example newtype wrapping 'T.Text' with validation.
newtype Validation = Validation T.Text
  deriving (Eq, Show)

-- | A Fleece schema for 'Validation' demonstrating validation and description usage.
validationSchema :: Fleece t => Schema t Validation
validationSchema =
  let
    maxLength = 12
    tooLongMsg = "At most " <> show maxLength <> " characters allowed"
    description =
      "Validated to be no more than " <> show maxLength <> " characters long."
  in
    describe (T.pack description) $
      validate
        (\(Validation t) -> t)
        ( \t ->
            if T.length t > maxLength
              then Left tooLongMsg
              else Right (Validation t)
        )
        text

-- | An example data type with an optional text field.
data OptionalField = OptionalField
  { exampleOptionalField :: Maybe T.Text
  -- ^ An optional text field.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'OptionalField' demonstrating optional field usage.
optionalFieldSchema :: Fleece t => Schema t OptionalField
optionalFieldSchema =
  object $
    constructor OptionalField
      #+ optional "optionalField" exampleOptionalField text

-- | An example data type with an optional nullable field that emits null when absent.
data OptionalNullableFieldEmitNull = OptionalNullableFieldEmitNull
  { exampleOptionalNullableFieldEmitNullField :: Maybe T.Text
  -- ^ An optional nullable field using 'EmitNull' encoding.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'OptionalNullableFieldEmitNull' using 'EmitNull' encoding.
optionalNullableFieldEmitNullSchema ::
  Fleece t =>
  Schema t OptionalNullableFieldEmitNull
optionalNullableFieldEmitNullSchema =
  object $
    constructor OptionalNullableFieldEmitNull
      #+ optionalNullable EmitNull "optionalNullableField" exampleOptionalNullableFieldEmitNullField text

-- | An example data type with an optional nullable field that omits the key when absent.
data OptionalNullableFieldOmitKey = OptionalNullableFieldOmitKey
  { exampleOptionalNullableFieldOmitKeyField :: Maybe T.Text
  -- ^ An optional nullable field using 'OmitKey' encoding.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'OptionalNullableFieldOmitKey' using 'OmitKey' encoding.
optionalNullableFieldOmitKeySchema ::
  Fleece t =>
  Schema t OptionalNullableFieldOmitKey
optionalNullableFieldOmitKeySchema =
  object $
    constructor OptionalNullableFieldOmitKey
      #+ optionalNullable OmitKey "optionalNullableField" exampleOptionalNullableFieldOmitKeyField text

-- | An example enum type with three values: Apple, Orange, and Kumquat.
data BoundedEnum
  = Apple
  | Orange
  | Kumquat
  deriving (Eq, Show, Enum, Bounded)

-- | A Fleece schema for 'BoundedEnum' demonstrating bounded enum usage.
boundedEnumSchema :: Fleece t => Schema t BoundedEnum
boundedEnumSchema =
  boundedEnum boundedEnumToText

-- | Converts a 'BoundedEnum' value to its text representation.
boundedEnumToText :: BoundedEnum -> T.Text
boundedEnumToText e =
  case e of
    Apple -> T.pack "apple"
    Orange -> T.pack "orange"
    Kumquat -> T.pack "kumquat"

-- | An example data type with defined fields and a catch-all for additional fields.
data AdditionalFieldsExample = AdditionalFieldsExample
  { field1 :: T.Text
  -- ^ A required text field.
  , field2 :: T.Text
  -- ^ A required text field.
  , otherFields :: Map.Map T.Text T.Text
  -- ^ A catch-all map for additional fields.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'AdditionalFieldsExample' demonstrating additional fields usage.
additionalFieldsExampleSchema :: Fleece t => Schema t AdditionalFieldsExample
additionalFieldsExampleSchema =
  object $
    constructor AdditionalFieldsExample
      #+ required "field1" field1 text
      #+ required "field2" field2 text
      #* additionalFields otherFields (describe (T.pack "Inline description.") text)

-- | An example data type with a number encoded as a JSON string.
data AbnormalNumbersExample = AbnormalNumbersExample
  { stringyNumber :: Scientific
  -- ^ A number encoded as a JSON string.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'AbnormalNumbersExample' demonstrating 'jsonString' usage.
abnormalNumbersExampleSchema :: Fleece t => Schema t AbnormalNumbersExample
abnormalNumbersExampleSchema =
  object $
    constructor AbnormalNumbersExample
      #+ required "stringyNumber" stringyNumber (jsonString number)

-- | An example data type containing a list of enum values.
newtype ListFieldExample = ListFieldExample
  { listField :: [BoundedEnum]
  -- ^ A list of 'BoundedEnum' values.
  }
  deriving (Eq, Show)

-- | A Fleece schema for 'ListFieldExample' demonstrating list field usage.
listFieldExampleSchema :: Fleece t => Schema t ListFieldExample
listFieldExampleSchema =
  object $
    constructor ListFieldExample
      #+ required "listField" listField (list boundedEnumSchema)

-- | An example anonymous union type of text and number values.
type UnionExample =
  Shrubbery.Union [T.Text, Scientific]

-- | A Fleece schema for 'UnionExample' demonstrating anonymous union usage.
unionExampleSchema :: Fleece t => Schema t UnionExample
unionExampleSchema =
  unionNamed (unqualifiedName "UnionExample") $
    unionMember text
      #| unionMember number

-- | An example tagged union type discriminated by a @"type"@ field.
type TaggedUnionExample =
  Shrubbery.TaggedUnion
    [ "person" @= Person
    , "company" @= Company
    ]

-- | A Fleece schema for 'TaggedUnionExample' demonstrating tagged union usage.
taggedUnionExampleSchema :: Fleece t => Schema t TaggedUnionExample
taggedUnionExampleSchema =
  taggedUnionNamed (unqualifiedName "TaggedUnionExample") "type" $
    taggedUnionMember @"person" personObject
      #@ taggedUnionMember @"company" companyObject

-- | An example data type for a person with a name and age, used in tagged union examples.
data Person = Person
  { personName :: T.Text
  -- ^ The person's name.
  , personAge :: Int
  -- ^ The person's age.
  }
  deriving (Eq, Show)

-- | A Fleece object definition for 'Person'.
personObject :: Fleece t => Object t Person Person
personObject =
  constructor Person
    #+ required "name" personName text
    #+ required "age" personAge int

-- | An example data type for a company with a name and a boolean flag, used in tagged union examples.
data Company = Company
  { companyName :: T.Text
  -- ^ The company's name.
  , companyIsToBigToFail :: Bool
  -- ^ Whether the company is too big to fail.
  }
  deriving (Eq, Show)

-- | A Fleece object definition for 'Company'.
companyObject :: Fleece t => Object t Company Company
companyObject =
  constructor Company
    #+ required "name" companyName text
    #+ required "tooBigToFail" companyIsToBigToFail boolean
