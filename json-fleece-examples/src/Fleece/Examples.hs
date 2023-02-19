module Fleece.Examples
  ( FooBar (..)
  , fooBarSchema
  , NullableFieldExample (..)
  , nullableFieldExampleSchema
  , ValidationExample (..)
  , validationExampleSchema
  , OptionalFieldExample (..)
  , optionalFieldExampleSchema
  , OptionalField_EmitNull_AcceptNull_Example (..)
  , optionalField_EmitNull_AcceptNull_ExampleSchema
  , OptionalField_OmitKey_AcceptNull_Example (..)
  , optionalField_OmitKey_AcceptNull_ExampleSchema
  , OptionalField_OmitKey_DelegateNull_Example (..)
  , optionalField_OmitKey_DelegateNull_ExampleSchema
  , OptionalField_OmitKey_DelegateNull_NullableExample (..)
  , optionalField_OmitKey_DelegateNull_NullableExampleSchema
  ) where

import Data.Scientific (Scientific)
import qualified Data.Text as T

import Fleece.Core
  ( Fleece
  , NullBehavior (EmitNull_AcceptNull, OmitKey_AcceptNull, OmitKey_DelegateNull)
  , constructor
  , nullable
  , number
  , object
  , optional
  , optionalField
  , required
  , text
  , validate
  , (#+)
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

data NullableFieldExample = NullableFieldExample
  { exampleNullableField :: Maybe T.Text
  }
  deriving (Eq, Show)

nullableFieldExampleSchema :: Fleece schema => schema NullableFieldExample
nullableFieldExampleSchema =
  object $
    constructor NullableFieldExample
      #+ required "nullableField" exampleNullableField (nullable text)

newtype ValidationExample = ValidationExample T.Text
  deriving (Eq, Show)

validationExampleSchema :: Fleece schema => schema ValidationExample
validationExampleSchema =
  validate
    (\(ValidationExample t) -> t)
    (\t -> if T.length t > 12 then Left "At most 12 characters allowed" else Right (ValidationExample t))
    text

data OptionalFieldExample = OptionalFieldExample
  { exampleOptionalField :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalFieldExampleSchema :: Fleece schema => schema OptionalFieldExample
optionalFieldExampleSchema =
  object $
    constructor OptionalFieldExample
      #+ optional "optionalField" exampleOptionalField text

data OptionalField_EmitNull_AcceptNull_Example = OptionalField_EmitNull_AcceptNull_Example
  { exampleOptional_EmitNull_AcceptNull_Field :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalField_EmitNull_AcceptNull_ExampleSchema ::
  Fleece schema =>
  schema OptionalField_EmitNull_AcceptNull_Example
optionalField_EmitNull_AcceptNull_ExampleSchema =
  object $
    constructor OptionalField_EmitNull_AcceptNull_Example
      #+ optionalField EmitNull_AcceptNull "optional_EmitNull_AcceptNull_Field" exampleOptional_EmitNull_AcceptNull_Field text

data OptionalField_OmitKey_AcceptNull_Example = OptionalField_OmitKey_AcceptNull_Example
  { exampleOptional_OmitKey_AcceptNull_Field :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalField_OmitKey_AcceptNull_ExampleSchema ::
  Fleece schema =>
  schema OptionalField_OmitKey_AcceptNull_Example
optionalField_OmitKey_AcceptNull_ExampleSchema =
  object $
    constructor OptionalField_OmitKey_AcceptNull_Example
      #+ optionalField OmitKey_AcceptNull "optional_OmitKey_AcceptNull_Field" exampleOptional_OmitKey_AcceptNull_Field text

data OptionalField_OmitKey_DelegateNull_Example = OptionalField_OmitKey_DelegateNull_Example
  { exampleOptional_OmitKey_DelegateNull_Field :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalField_OmitKey_DelegateNull_ExampleSchema ::
  Fleece schema =>
  schema OptionalField_OmitKey_DelegateNull_Example
optionalField_OmitKey_DelegateNull_ExampleSchema =
  object $
    constructor OptionalField_OmitKey_DelegateNull_Example
      #+ optionalField OmitKey_DelegateNull "optional_OmitKey_DelegateNull_Field" exampleOptional_OmitKey_DelegateNull_Field text

data OptionalField_OmitKey_DelegateNull_NullableExample = OptionalField_OmitKey_DelegateNull_NullableExample
  { exampleOptional_OmitKey_DelegateNull_NullableField :: Maybe (Maybe T.Text)
  }
  deriving (Eq, Show)

optionalField_OmitKey_DelegateNull_NullableExampleSchema ::
  Fleece schema =>
  schema OptionalField_OmitKey_DelegateNull_NullableExample
optionalField_OmitKey_DelegateNull_NullableExampleSchema =
  object $
    constructor OptionalField_OmitKey_DelegateNull_NullableExample
      #+ optionalField
        OmitKey_DelegateNull
        "optional_OmitKey_DelegateNull_Nullable_Field"
        exampleOptional_OmitKey_DelegateNull_NullableField
        (nullable text)
