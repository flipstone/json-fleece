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
  , EmbeddedObjectParent (..)
  , embeddedObjectParentSchema
  , EmbeddedObjectChild (..)
  ) where

import Data.Scientific (Scientific)
import qualified Data.Text as T

import Fleece.Core
  ( Fleece
  , NothingEncoding (EmitNull, OmitKey)
  , Null
  , Object
  , boundedEnum
  , constructor
  , embedded
  , nullable
  , number
  , object
  , optional
  , optionalNullable
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
