{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Core.Schemas
  ( optionalNullable
  , object
  , boundedEnum
  , validate
  , list
  , Fleece.Core.Schemas.map
  , nonEmpty
  , nonEmptyText
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
  , fixed
  , fixedNamed
  , string
  , utcTime
  , utcTimeWithFormat
  , localTime
  , localTimeWithFormat
  , zonedTime
  , zonedTimeWithFormat
  , day
  , dayWithFormat
  , timeWithFormat
  , boundedIntegralNumber
  , boundedIntegralNumberNamed
  , unboundedIntegralNumber
  , unboundedIntegralNumberNamed
  , transform
  , transformNamed
  , coerceSchema
  , coerceSchemaNamed
  , eitherOf
  , eitherOfNamed
  , union
  , unionMember
  , taggedUnion
  , taggedUnionMember
  , bareOrJSONString
  , set
  , SetDuplicateHandling (AllowInputDuplicates, RejectInputDuplicates)
  , NothingEncoding (EmitNull, OmitKey)
  ) where

import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.Time as AttoTime
import Data.Coerce (Coercible, coerce)
import Data.Fixed (Fixed, HasResolution (resolution))
import qualified Data.Int as I
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.NonEmptyText as NET
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific, base10Exponent, floatingOrInteger, fromFloatDigits, normalize, toBoundedInteger, toRealFloat)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Word as W
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol)
import Shrubbery (Tag, TagIndex, TagType, TaggedTypes, TaggedUnion, TypeAtIndex, Union, branch, branchBuild, branchEnd, dissectUnion, firstIndexOfType, index0, index1, unify, unifyWithIndex, type (@=))
import Shrubbery.TypeList (FirstIndexOf, Length)

import Fleece.Core.Class
  ( Field
  , Fleece
  , Null (Null)
  , Object
  , TaggedUnionMembers
  , UnionMembers
  , additionalFields
  , array
  , boundedEnumNamed
  , constructor
  , jsonString
  , nullable
  , number
  , objectNamed
  , optional
  , taggedUnionMemberWithTag
  , taggedUnionNamed
  , text
  , unionCombine
  , unionMemberWithIndex
  , unionNamed
  , validateNamed
  , (#*)
  , (#|)
  )
import Fleece.Core.Name
  ( Name
  , annotateName
  , defaultSchemaName
  , nameToString
  , nameUnqualified
  , unqualifiedName
  )
import Fleece.Core.SchemaName (schemaName)

eitherOf ::
  forall schema a b.
  ( Fleece schema
  , FirstIndexOf b '[a, b] ~ 1
  ) =>
  (forall anySchema. Fleece anySchema => anySchema a) ->
  (forall anySchema. Fleece anySchema => anySchema b) ->
  schema (Either a b)
eitherOf leftSchema rightSchema =
  let
    name =
      unqualifiedName
        ( "Either "
            <> nameToString (schemaName leftSchema)
            <> " "
            <> nameToString (schemaName rightSchema)
        )
  in
    eitherOfNamed name leftSchema rightSchema

eitherOfNamed ::
  forall schema a b.
  ( Fleece schema
  , FirstIndexOf b '[a, b] ~ 1
  ) =>
  Name ->
  (forall anySchema. Fleece anySchema => anySchema a) ->
  (forall anySchema. Fleece anySchema => anySchema b) ->
  schema (Either a b)
eitherOfNamed name leftSchema rightSchema =
  let
    toUnion :: Either a b -> Union '[a, b]
    toUnion eitherAOrB =
      case eitherAOrB of
        Left a -> unify a
        Right b -> unify b

    fromUnion :: Union '[a, b] -> Either a b
    fromUnion =
      dissectUnion
        . branchBuild
        . branch Left
        . branch Right
        $ branchEnd

    unionSchema :: forall anySchema. Fleece anySchema => anySchema (Union '[a, b])
    unionSchema =
      unionNamed name $
        unionMember leftSchema
          #| unionMember rightSchema
  in
    transformNamed
      name
      toUnion
      fromUnion
      unionSchema

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
  (forall anySchema. Fleece anySchema => anySchema a) ->
  UnionMembers schema types '[a]
unionMember =
  unionMemberWithIndex firstIndexOfType

taggedUnion ::
  forall (tags :: [Tag]) schema.
  (Typeable tags, Fleece schema, KnownNat (Length (TaggedTypes tags))) =>
  String ->
  TaggedUnionMembers schema tags tags ->
  schema (TaggedUnion tags)
taggedUnion tagProperty members =
  let
    name =
      defaultSchemaName schema

    schema =
      taggedUnionNamed name tagProperty members
  in
    schema

taggedUnionMember ::
  forall (tag :: Symbol) (tags :: [Tag]) schema a n.
  ( KnownSymbol tag
  , n ~ TagIndex tag tags
  , KnownNat n
  , TagType tag tags ~ a
  , TypeAtIndex n (TaggedTypes tags) ~ a
  ) =>
  Fleece schema =>
  Object schema a a ->
  TaggedUnionMembers schema tags '[tag @= a]
taggedUnionMember =
  let
    tagProxy :: Proxy tag
    tagProxy = Proxy
  in
    taggedUnionMemberWithTag tagProxy

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
  (forall anySchema. Fleece anySchema => anySchema b) ->
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
  (forall anySchema. Fleece anySchema => anySchema b) ->
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
  (forall anySchema. Fleece anySchema => anySchema b) ->
  schema a
transformNamed name aToB bToA =
  validateNamed name aToB (Right . bToA)

coerceSchema ::
  (Fleece schema, Typeable a, Coercible a b) =>
  (forall anySchema. Fleece anySchema => anySchema b) ->
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
  (forall anySchema. Fleece anySchema => anySchema b) ->
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
  (objectType -> Maybe a) ->
  (forall anySchema. Fleece anySchema => anySchema a) ->
  Field schema objectType (Maybe a)
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

list :: forall schema a. Fleece schema => (forall anySchema. Fleece anySchema => anySchema a) -> schema [a]
list itemSchema =
  transformNamed
    (unqualifiedName $ "[" <> nameUnqualified (schemaName itemSchema) <> "]")
    V.fromList
    V.toList
    (array itemSchema)

map :: (Fleece schema, Typeable a) => (forall anySchema. Fleece anySchema => anySchema a) -> schema (Map.Map T.Text a)
map innerSchema =
  object $
    constructor id
      #* additionalFields id innerSchema

nonEmpty :: forall schema a. Fleece schema => (forall anySchema. Fleece anySchema => anySchema a) -> schema (NEL.NonEmpty a)
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

data SetDuplicateHandling
  = AllowInputDuplicates
  | RejectInputDuplicates

set :: forall schema a. (Ord a, Fleece schema) => SetDuplicateHandling -> (forall anySchema. Fleece anySchema => anySchema a) -> schema (Set.Set a)
set handling itemSchema =
  case handling of
    AllowInputDuplicates ->
      transformNamed
        (unqualifiedName $ "Set [" <> nameUnqualified (schemaName itemSchema) <> "]")
        (V.fromList . Set.toList)
        (Set.fromList . V.toList)
        (array itemSchema)
    RejectInputDuplicates ->
      let
        validateNoDuplicates items =
          case Set.fromList $ V.toList items of
            itemSet
              | Set.size itemSet == length items ->
                  Right itemSet
            _ ->
              Left "Unexpected duplicates found in input"
      in
        validateNamed
          (unqualifiedName $ "Set [" <> nameUnqualified (schemaName itemSchema) <> "]")
          (V.fromList . Set.toList)
          validateNoDuplicates
          (array itemSchema)

nonEmptyText :: Fleece schema => schema NET.NonEmptyText
nonEmptyText =
  let
    validateNonEmptyText value =
      case NET.fromText value of
        Just net -> Right net
        Nothing -> Left "Expected non-empty text for NonEmptyText, but text was empty"
  in
    validateNamed
      (unqualifiedName "NonEmptyText")
      NET.toText
      validateNonEmptyText
      text

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

-- | Validates a 'number', failing if the precision is too high for the given resolution.
fixed ::
  (Fleece schema, HasResolution r, Typeable r) =>
  schema (Fixed r)
fixed =
  let
    name =
      defaultSchemaName schema

    schema =
      fixedNamed name
  in
    schema

-- | Validates a 'number', failing if the precision is too high for the given resolution.
fixedNamed ::
  (Fleece schema, HasResolution r) =>
  Name ->
  schema (Fixed r)
fixedNamed name =
  validateNamed
    name
    realToFrac
    (fixedFromScientific name)
    number

fixedFromScientific :: forall r. HasResolution r => Name -> Scientific -> Either String (Fixed r)
fixedFromScientific name sci =
  let
    normalized = normalize sci
    fixedResolution = resolution (Proxy :: Proxy r)
    sciExponent = base10Exponent normalized
  in
    if sciExponent >= 0 || 10 ^ abs sciExponent <= fixedResolution
      then Right (realToFrac normalized)
      else
        Left $
          "Invalid fixed precision number "
            <> nameToString name
            <> ", max precision is "
            <> show (floor (logBase 10 (fromInteger fixedResolution) :: Double) :: Integer)
            <> " decimal places."

string :: Fleece schema => schema String
string = transform T.pack T.unpack text

utcTime :: Fleece schema => schema Time.UTCTime
utcTime =
  iso8601Formatted "UTCTime" ISO8601.iso8601Format AttoTime.utcTime

utcTimeWithFormat :: Fleece schema => String -> schema Time.UTCTime
utcTimeWithFormat = timeWithFormat "UTCTime"

localTime :: Fleece schema => schema Time.LocalTime
localTime =
  iso8601Formatted "LocalTime" ISO8601.iso8601Format AttoTime.localTime

localTimeWithFormat :: Fleece schema => String -> schema Time.LocalTime
localTimeWithFormat = timeWithFormat "LocalTime"

zonedTime :: Fleece schema => schema Time.ZonedTime
zonedTime =
  iso8601Formatted "ZonedTime" ISO8601.iso8601Format AttoTime.zonedTime

zonedTimeWithFormat :: Fleece schema => String -> schema Time.ZonedTime
zonedTimeWithFormat = timeWithFormat "ZonedTime"

day :: Fleece schema => schema Time.Day
day =
  iso8601Formatted "Day" ISO8601.iso8601Format AttoTime.day

dayWithFormat :: Fleece schema => String -> schema Time.Day
dayWithFormat = timeWithFormat "Day"

timeWithFormat :: (Time.FormatTime t, Time.ParseTime t) => Fleece schema => String -> String -> schema t
timeWithFormat typeName formatString =
  let
    decode raw =
      case Time.parseTimeM False Time.defaultTimeLocale formatString raw of
        Just success -> Right success
        Nothing ->
          Left $
            "Invalid " <> typeName <> ", custom format is: " <> formatString
  in
    validateNamed
      (unqualifiedName $ typeName <> " in " <> formatString <> " format")
      (Time.formatTime Time.defaultTimeLocale formatString)
      decode
      string

bareOrJSONString :: forall schema a. Fleece schema => (forall anySchema. Fleece anySchema => anySchema a) -> schema a
bareOrJSONString baseSchema =
  let
    toUnion :: a -> Union '[a, a]
    toUnion =
      unifyWithIndex index0

    fromUnion :: Union '[a, a] -> a
    fromUnion =
      dissectUnion
        . branchBuild
        . branch id
        . branch id
        $ branchEnd

    name =
      annotateName
        (schemaName baseSchema)
        "(bare or encoded as json string)"

    unionSchema :: forall anySchema. Fleece anySchema => anySchema (Union '[a, a])
    unionSchema =
      unionNamed name $
        unionCombine
          (unionMemberWithIndex index0 baseSchema)
          (unionMemberWithIndex index1 (jsonString baseSchema))
  in
    transformNamed
      name
      toUnion
      fromUnion
      unionSchema

-- An internal helper for building building time schemes
iso8601Formatted ::
  Fleece schema =>
  String ->
  ISO8601.Format t ->
  AttoText.Parser t ->
  schema t
iso8601Formatted name format parser =
  let
    parseTime jsonText =
      case AttoText.parseOnly (parser <* AttoText.endOfInput) jsonText of
        Right time ->
          Right time
        Left err ->
          Left $
            "Invalid time format for "
              <> name
              <> " value "
              <> show jsonText
              <> ": "
              <> err
  in
    validateNamed
      (unqualifiedName name)
      (T.pack . ISO8601.formatShow format)
      parseTime
      text
