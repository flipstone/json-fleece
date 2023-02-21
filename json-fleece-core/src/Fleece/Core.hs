{-# LANGUAGE TypeFamilies #-}

module Fleece.Core
  ( Fleece
      ( Field
      , Object
      , text
      , number
      , boolean
      , array
      , null
      , required
      , optionalField
      , objectNamed
      , constructor
      , nullable
      , field
      , validateNamed
      , boundedEnumNamed
      )
  , optional
  , object
  , boundedEnum
  , validate
  , list
  , nonEmpty
  , int
  , int8
  , int16
  , int32
  , int64
  , word
  , word8
  , word16
  , word32
  , word64
  , string
  , utcTime
  , localTime
  , zonedTime
  , day
  , iso8601Formatted
  , boundedIntegralNumber
  , boundedIntegralNumberNamed
  , transform
  , transformNamed
  , coerceSchema
  , coerceSchemaNamed
  , (#+)
  , Null (Null)
  , NullBehavior
    ( EmitNull_AcceptNull
    , OmitKey_AcceptNull
    , OmitKey_DelegateNull
    )
  ) where

import Data.Coerce (Coercible, coerce)
import qualified Data.Int as I
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NEL
import Data.Scientific (Scientific, toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import qualified Data.Vector as V
import qualified Data.Word as W

data Null
  = Null
  deriving (Eq, Show)

class Fleece schema where
  data Object schema :: Type -> Type -> Type
  data Field schema :: Type -> Type -> Type

  number :: schema Scientific

  text :: schema T.Text

  boolean :: schema Bool

  array :: schema a -> schema (V.Vector a)

  null :: schema Null

  nullable :: schema a -> schema (Maybe a)

  required ::
    String ->
    (object -> a) ->
    schema a ->
    Field schema object a

  optionalField ::
    NullBehavior ->
    String ->
    (object -> Maybe a) ->
    schema a ->
    Field schema object (Maybe a)

  objectNamed ::
    String ->
    Object schema a a ->
    schema a

  constructor ::
    constructor ->
    Object schema object constructor

  field ::
    Object schema object (a -> b) ->
    Field schema object a ->
    Object schema object b

  validateNamed ::
    String ->
    (a -> b) ->
    (b -> Either String a) ->
    (schema b) ->
    (schema a)

  boundedEnumNamed ::
    (Bounded a, Enum a) =>
    String ->
    (a -> T.Text) ->
    schema a

(#+) ::
  Fleece schema =>
  Object schema object (a -> b) ->
  Field schema object a ->
  Object schema object b
(#+) =
  field

infixl 9 #+

data NullBehavior
  = EmitNull_AcceptNull
  | OmitKey_AcceptNull
  | OmitKey_DelegateNull

object ::
  (Fleece schema, Typeable a) =>
  Object schema a a ->
  schema a
object o =
  let
    name =
      defaultSchemaName schema

    schema =
      objectNamed name o
  in
    schema

boundedEnum ::
  (Fleece schema, Typeable a, Enum a, Bounded a) =>
  (a -> T.Text) ->
  schema a
boundedEnum toText =
  let
    name =
      defaultSchemaName schema

    schema =
      boundedEnumNamed name toText
  in
    schema

validate ::
  (Fleece schema, Typeable a) =>
  (a -> b) ->
  (b -> Either String a) ->
  schema b ->
  schema a
validate uncheck check schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      validateNamed name uncheck check schemaB
  in
    schemaA

transform ::
  (Fleece schema, Typeable a) =>
  (a -> b) ->
  (b -> a) ->
  schema b ->
  schema a
transform aToB bToA schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      transformNamed name aToB bToA schemaB
  in
    schemaA

transformNamed ::
  Fleece schema =>
  String ->
  (a -> b) ->
  (b -> a) ->
  schema b ->
  schema a
transformNamed name aToB bToA =
  validateNamed name aToB (Right . bToA)

coerceSchema ::
  (Fleece schema, Typeable a, Coercible a b) =>
  schema b ->
  schema a
coerceSchema schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      coerceSchemaNamed name schemaB
  in
    schemaA

coerceSchemaNamed ::
  (Fleece schema, Coercible a b) =>
  String ->
  schema b ->
  schema a
coerceSchemaNamed name schemaB =
  transformNamed name coerce coerce schemaB

optional ::
  Fleece schema =>
  String ->
  (object -> Maybe a) ->
  schema a ->
  Field schema object (Maybe a)
optional = optionalField OmitKey_AcceptNull

list :: Fleece schema => schema a -> schema [a]
list itemSchema =
  transformNamed
    "List"
    V.fromList
    V.toList
    (array itemSchema)

nonEmpty :: Fleece schema => schema a -> schema (NEL.NonEmpty a)
nonEmpty itemSchema =
  let
    validateNonEmpty items =
      case NEL.nonEmpty items of
        Just nonEmptyItems -> Right nonEmptyItems
        Nothing -> Left "Expected non-empty array for NonEmpty list, but array was empty"
  in
    validateNamed
      "NonEmpty"
      NEL.toList
      validateNonEmpty
      (list itemSchema)

boundedIntegralNumberNamed ::
  (Fleece schema, Integral n, Bounded n) =>
  String ->
  schema n
boundedIntegralNumberNamed name =
  let
    validateInteger s =
      case toBoundedInteger s of
        Just n -> pure n
        Nothing ->
          Left $
            "Error parsing bounded integer value for "
              <> name
              <> ". Value not integral, or exceeds the bounds of the expected type: "
              <> show s
  in
    validateNamed
      name
      fromIntegral
      validateInteger
      number

boundedIntegralNumber ::
  (Fleece schema, Integral n, Bounded n, Typeable n) =>
  schema n
boundedIntegralNumber =
  let
    name =
      defaultSchemaName schema

    schema =
      boundedIntegralNumberNamed name
  in
    schema

int :: Fleece schema => schema Int
int = boundedIntegralNumber

int8 :: Fleece schema => schema I.Int8
int8 = boundedIntegralNumber

int16 :: Fleece schema => schema I.Int16
int16 = boundedIntegralNumber

int32 :: Fleece schema => schema I.Int32
int32 = boundedIntegralNumber

int64 :: Fleece schema => schema I.Int64
int64 = boundedIntegralNumber

word :: Fleece schema => schema Word
word = boundedIntegralNumber

word8 :: Fleece schema => schema W.Word8
word8 = boundedIntegralNumber

word16 :: Fleece schema => schema W.Word16
word16 = boundedIntegralNumber

word32 :: Fleece schema => schema W.Word32
word32 = boundedIntegralNumber

word64 :: Fleece schema => schema W.Word64
word64 = boundedIntegralNumber

string :: Fleece schema => schema String
string = transform T.pack T.unpack text

utcTime :: Fleece schema => schema Time.UTCTime
utcTime =
  iso8601Formatted ISO8601.iso8601Format

localTime :: Fleece schema => schema Time.LocalTime
localTime =
  iso8601Formatted ISO8601.iso8601Format

zonedTime :: Fleece schema => schema Time.ZonedTime
zonedTime =
  iso8601Formatted ISO8601.iso8601Format

day :: Fleece schema => schema Time.Day
day =
  iso8601Formatted ISO8601.iso8601Format

iso8601Formatted ::
  (Fleece schema, Typeable t) =>
  ISO8601.Format t ->
  schema t
iso8601Formatted format =
  let
    name =
      defaultSchemaName format

    parseTime jsonText =
      case ISO8601.formatParseM format (T.unpack jsonText) of
        Just time -> pure time
        Nothing ->
          Left $
            "Invalid time format for "
              <> name
              <> ": "
              <> show jsonText
  in
    validate
      (T.pack . ISO8601.formatShow format)
      parseTime
      text

-- Internal Helpers

defaultSchemaName ::
  Typeable a =>
  schema a ->
  String
defaultSchemaName =
  tyConName
    . typeRepTyCon
    . typeRep
