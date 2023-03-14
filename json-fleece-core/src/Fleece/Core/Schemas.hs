{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Fleece.Core.Schemas
  ( optionalNullable
  , object
  , boundedEnum
  , validate
  , list
  , Fleece.Core.Schemas.map
  , nonEmpty
  , integer
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
  , double
  , float
  , realFloat
  , realFloatNamed
  , string
  , utcTime
  , localTime
  , zonedTime
  , day
  , iso8601Formatted
  , boundedIntegralNumber
  , boundedIntegralNumberNamed
  , unboundedIntegralNumber
  , unboundedIntegralNumberNamed
  , transform
  , transformNamed
  , coerceSchema
  , coerceSchemaNamed
  , union
  , unionMember
  , NothingEncoding (EmitNull, OmitKey)
  ) where

import Data.Coerce (Coercible, coerce)
import qualified Data.Int as I
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Data.Scientific (floatingOrInteger, fromFloatDigits, toBoundedInteger, toRealFloat)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Word as W
import GHC.TypeLits (KnownNat)
import Shrubbery (Union, firstIndexOfType)
import Shrubbery.TypeList (FirstIndexOf, Length)

import Fleece.Core.Class
  ( Field
  , Fleece
  , Null (Null)
  , Object
  , UnionMembers
  , additionalFields
  , array
  , boundedEnumNamed
  , constructor
  , nullable
  , number
  , objectNamed
  , optional
  , schemaName
  , text
  , unionMemberWithIndex
  , unionNamed
  , validateNamed
  , (#*)
  )
import Fleece.Core.Name
  ( Name
  , defaultSchemaName
  , nameToString
  , nameUnqualified
  , unqualifiedName
  )

union ::
  (Typeable types, Fleece schema, KnownNat (Length types)) =>
  UnionMembers schema types types ->
  schema (Union types)
union members =
  let
    name =
      defaultSchemaName schema

    schema =
      unionNamed name members
  in
    schema

unionMember ::
  ( Fleece schema
  , KnownNat branchIndex
  , branchIndex ~ FirstIndexOf a types
  ) =>
  schema a ->
  UnionMembers schema types '[a]
unionMember =
  unionMemberWithIndex firstIndexOfType

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
  Name ->
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
  Name ->
  schema b ->
  schema a
coerceSchemaNamed name schemaB =
  transformNamed name coerce coerce schemaB

data NothingEncoding
  = EmitNull
  | OmitKey

optionalNullable ::
  Fleece schema =>
  NothingEncoding ->
  String ->
  (object -> Maybe a) ->
  schema a ->
  Field schema object (Maybe a)
optionalNullable encoding name accessor schema =
  let
    nullableAccessor o =
      case accessor o of
        Just a -> Just (Right a)
        Nothing ->
          case encoding of
            OmitKey -> Nothing
            EmitNull -> Just (Left Null)

    collapseNull mbNullOrA =
      either (\Null -> Nothing) Just =<< mbNullOrA
  in
    fmap collapseNull $
      optional name nullableAccessor (nullable schema)

list :: Fleece schema => schema a -> schema [a]
list itemSchema =
  transformNamed
    (unqualifiedName $ "[" <> nameUnqualified (schemaName itemSchema) <> "]")
    V.fromList
    V.toList
    (array itemSchema)

map :: (Fleece schema, Typeable a) => schema a -> schema (Map.Map T.Text a)
map innerSchema =
  object $
    constructor id
      #* additionalFields id innerSchema

nonEmpty :: Fleece schema => schema a -> schema (NEL.NonEmpty a)
nonEmpty itemSchema =
  let
    validateNonEmpty items =
      case NEL.nonEmpty items of
        Just nonEmptyItems -> Right nonEmptyItems
        Nothing -> Left "Expected non-empty array for NonEmpty list, but array was empty"
  in
    validateNamed
      (unqualifiedName $ "NonEmpty " <> nameUnqualified (schemaName itemSchema))
      NEL.toList
      validateNonEmpty
      (list itemSchema)

integer :: Fleece schema => schema Integer
integer =
  unboundedIntegralNumber

unboundedIntegralNumberNamed ::
  (Fleece schema, Integral n) =>
  Name ->
  schema n
unboundedIntegralNumberNamed name =
  let
    asDouble :: Double -> a -> a
    asDouble _ = id

    validateInteger s =
      case floatingOrInteger s of
        Right n -> pure n
        Left f ->
          asDouble f $
            Left $
              "Error parsing bounded integer value for "
                <> nameToString name
                <> ". Value not integral: "
                <> show s
  in
    validateNamed
      name
      fromIntegral
      validateInteger
      number

unboundedIntegralNumber ::
  (Fleece schema, Integral n, Typeable n) =>
  schema n
unboundedIntegralNumber =
  let
    name =
      defaultSchemaName schema

    schema =
      unboundedIntegralNumberNamed name
  in
    schema

boundedIntegralNumberNamed ::
  (Fleece schema, Integral n, Bounded n) =>
  Name ->
  schema n
boundedIntegralNumberNamed name =
  let
    validateInteger s =
      case toBoundedInteger s of
        Just n -> pure n
        Nothing ->
          Left $
            "Error parsing bounded integer value for "
              <> nameToString name
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

double :: Fleece schema => schema Double
double =
  realFloat

float :: Fleece schema => schema Float
float =
  realFloat

realFloat ::
  (Fleece schema, RealFloat f, Typeable f) =>
  schema f
realFloat =
  let
    name =
      defaultSchemaName schema

    schema =
      realFloatNamed name
  in
    schema

realFloatNamed ::
  (Fleece schema, RealFloat f) =>
  Name ->
  schema f
realFloatNamed name =
  transformNamed
    name
    fromFloatDigits
    toRealFloat
    number

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
              <> nameToString name
              <> ": "
              <> show jsonText
  in
    validate
      (T.pack . ISO8601.formatShow format)
      parseTime
      text
