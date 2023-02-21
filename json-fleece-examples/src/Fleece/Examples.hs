module Fleece.Examples
  ( FooBar (..)
  , fooBarSchema
  , NullableField (..)
  , nullableFieldSchema
  , Validation (..)
  , validationSchema
  , OptionalField (..)
  , optionalFieldSchema
  , OptionalField_EmitNull_AcceptNull (..)
  , optionalField_EmitNull_AcceptNullSchema
  , OptionalField_OmitKey_AcceptNull (..)
  , optionalField_OmitKey_AcceptNullSchema
  , OptionalField_OmitKey_DelegateNull (..)
  , optionalField_OmitKey_DelegateNullSchema
  , OptionalField_OmitKey_DelegateNull_Nullable (..)
  , optionalField_OmitKey_DelegateNull_NullableSchema
  , BoundedEnum (..)
  , boundedEnumSchema
  , boundedEnumToText
  , EmbeddedObjectParent (..)
  , embeddedObjectParentSchema
  , EmbeddedObjectChild (..)
  ) where

import Data.Scientific (Scientific)
import qualified Data.Text as T

import Fleece.Core
  ( Fleece
  , NullBehavior (EmitNull_AcceptNull, OmitKey_AcceptNull, OmitKey_DelegateNull)
  , Object
  , boundedEnum
  , constructor
  , embedded
  , nullable
  , number
  , object
  , optional
  , optionalField
  , required
  , text
  , validate
  , (##)
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

data NullableField = NullableField
  { exampleNullableField :: Maybe T.Text
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

data OptionalField_EmitNull_AcceptNull = OptionalField_EmitNull_AcceptNull
  { exampleOptional_EmitNull_AcceptNull_Field :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalField_EmitNull_AcceptNullSchema ::
  Fleece schema =>
  schema OptionalField_EmitNull_AcceptNull
optionalField_EmitNull_AcceptNullSchema =
  object $
    constructor OptionalField_EmitNull_AcceptNull
      #+ optionalField EmitNull_AcceptNull "optional_EmitNull_AcceptNull_Field" exampleOptional_EmitNull_AcceptNull_Field text

data OptionalField_OmitKey_AcceptNull = OptionalField_OmitKey_AcceptNull
  { exampleOptional_OmitKey_AcceptNull_Field :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalField_OmitKey_AcceptNullSchema ::
  Fleece schema =>
  schema OptionalField_OmitKey_AcceptNull
optionalField_OmitKey_AcceptNullSchema =
  object $
    constructor OptionalField_OmitKey_AcceptNull
      #+ optionalField OmitKey_AcceptNull "optional_OmitKey_AcceptNull_Field" exampleOptional_OmitKey_AcceptNull_Field text

data OptionalField_OmitKey_DelegateNull = OptionalField_OmitKey_DelegateNull
  { exampleOptional_OmitKey_DelegateNull_Field :: Maybe T.Text
  }
  deriving (Eq, Show)

optionalField_OmitKey_DelegateNullSchema ::
  Fleece schema =>
  schema OptionalField_OmitKey_DelegateNull
optionalField_OmitKey_DelegateNullSchema =
  object $
    constructor OptionalField_OmitKey_DelegateNull
      #+ optionalField OmitKey_DelegateNull "optional_OmitKey_DelegateNull_Field" exampleOptional_OmitKey_DelegateNull_Field text

data OptionalField_OmitKey_DelegateNull_Nullable = OptionalField_OmitKey_DelegateNull_Nullable
  { exampleOptional_OmitKey_DelegateNull_NullableField :: Maybe (Maybe T.Text)
  }
  deriving (Eq, Show)

optionalField_OmitKey_DelegateNull_NullableSchema ::
  Fleece schema =>
  schema OptionalField_OmitKey_DelegateNull_Nullable
optionalField_OmitKey_DelegateNull_NullableSchema =
  object $
    constructor OptionalField_OmitKey_DelegateNull_Nullable
      #+ optionalField
        OmitKey_DelegateNull
        "optional_OmitKey_DelegateNull_Nullable_Field"
        exampleOptional_OmitKey_DelegateNull_NullableField
        (nullable text)

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

data EmbeddedObjectParent = EmbeddedObjectParent
  { parentField :: T.Text
  , child :: EmbeddedObjectChild
  }
  deriving (Eq, Show)

embeddedObjectParentSchema :: Fleece schema => schema EmbeddedObjectParent
embeddedObjectParentSchema =
  object $
    constructor EmbeddedObjectParent
      #+ required "parentField" parentField text
      ## embedded child embeddedObjectChildObject

embeddedObjectChildObject ::
  Fleece schema =>
  Object schema EmbeddedObjectChild EmbeddedObjectChild
embeddedObjectChildObject =
  constructor EmbeddedObjectChild
    #+ required "childField" childField text

data EmbeddedObjectChild = EmbeddedObjectChild
  { childField :: T.Text
  }
  deriving (Eq, Show)
