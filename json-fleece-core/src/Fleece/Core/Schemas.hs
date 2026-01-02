{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , fixed
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
  , coerceSchema
  , coerceSchemaNamed
  , coerceSchemaAnonymous
  , eitherOf
  , eitherOfNamed
  , union
  , unionMember
  , taggedUnion
  , taggedUnionMember
  , set
  , SetDuplicateHandling (AllowInputDuplicates, RejectInputDuplicates)
  , NothingEncoding (EmitNull, OmitKey)
  ) where

import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.Time as AttoTime
import Data.Coerce (Coercible, coerce)
import Data.Fixed (Fixed, HasResolution (resolution))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.NonEmptyText as NET
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (Scientific, base10Exponent, normalize)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol)
import Shrubbery (Tag, TagIndex, TagType, TaggedTypes, TaggedUnion, TypeAtIndex, Union, branch, branchBuild, branchEnd, dissectUnion, firstIndexOfType, unify, type (@=))
import Shrubbery.TypeList (FirstIndexOf, Length)

import Fleece.Core.Class
  ( Field
  , Fleece
  , Null (Null)
  , Object
  , Schema
  , TaggedUnionMembers
  , UnionMembers
  , additionalFields
  , array
  , boundedEnumNamed
  , constructor
  , format
  , nullable
  , number
  , objectNamed
  , optional
  , schemaName
  , taggedUnionMemberWithTag
  , taggedUnionNamed
  , text
  , transformAnonymous
  , transformNamed
  , unboundedIntegralNumber
  , unionMemberWithIndex
  , unionNamed
  , validateAnonymous
  , validateNamed
  , (#*)
  , (#|)
  )
import Fleece.Core.Name
  ( Name
  , defaultSchemaName
  , nameToString
  , unqualifiedName
  )

eitherOf ::
  forall t a b.
  ( Fleece t
  , FirstIndexOf b '[a, b] ~ 1
  ) =>
  Schema t a ->
  Schema t b ->
  Schema t (Either a b)
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
  forall t a b.
  ( Fleece t
  , FirstIndexOf b '[a, b] ~ 1
  ) =>
  Name ->
  Schema t a ->
  Schema t b ->
  Schema t (Either a b)
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

    unionSchema :: Schema t (Union '[a, b])
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
  (Typeable types, Fleece t, KnownNat (Length types)) =>
  UnionMembers t types types ->
  Schema t (Union types)
union members =
  let
    name =
      defaultSchemaName schema

    schema =
      unionNamed name members
  in
    schema

unionMember ::
  ( Fleece t
  , KnownNat branchIndex
  , branchIndex ~ FirstIndexOf a types
  ) =>
  Schema t a ->
  UnionMembers t types '[a]
unionMember =
  unionMemberWithIndex firstIndexOfType

taggedUnion ::
  forall (tags :: [Tag]) t.
  (Typeable tags, Fleece t, KnownNat (Length (TaggedTypes tags))) =>
  String ->
  TaggedUnionMembers t tags tags ->
  Schema t (TaggedUnion tags)
taggedUnion tagProperty members =
  let
    name =
      defaultSchemaName schema

    schema =
      taggedUnionNamed name tagProperty members
  in
    schema

taggedUnionMember ::
  forall (tag :: Symbol) (tags :: [Tag]) t a n.
  ( KnownSymbol tag
  , n ~ TagIndex tag tags
  , KnownNat n
  , TagType tag tags ~ a
  , TypeAtIndex n (TaggedTypes tags) ~ a
  ) =>
  Fleece t =>
  Object t a a ->
  TaggedUnionMembers t tags '[tag @= a]
taggedUnionMember =
  let
    tagProxy :: Proxy tag
    tagProxy = Proxy
  in
    taggedUnionMemberWithTag tagProxy

object ::
  (Fleece t, Typeable a) =>
  Object t a a ->
  Schema t a
object o =
  let
    name =
      defaultSchemaName schema

    schema =
      objectNamed name o
  in
    schema

boundedEnum ::
  (Fleece t, Typeable a, Enum a, Bounded a) =>
  (a -> T.Text) ->
  Schema t a
boundedEnum toText =
  let
    name =
      defaultSchemaName schema

    schema =
      boundedEnumNamed name toText
  in
    schema

validate ::
  (Fleece t, Typeable a) =>
  (a -> b) ->
  (b -> Either String a) ->
  Schema t b ->
  Schema t a
validate uncheck check schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      validateNamed name uncheck check schemaB
  in
    schemaA

coerceSchema ::
  (Fleece t, Typeable a, Coercible a b) =>
  Schema t b ->
  Schema t a
coerceSchema schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      coerceSchemaNamed name schemaB
  in
    schemaA

coerceSchemaNamed ::
  (Fleece t, Coercible a b) =>
  Name ->
  Schema t b ->
  Schema t a
coerceSchemaNamed name schemaB =
  transformNamed name coerce coerce schemaB

coerceSchemaAnonymous ::
  (Fleece t, Coercible a b) =>
  Schema t b ->
  Schema t a
coerceSchemaAnonymous schemaB =
  transformAnonymous coerce coerce schemaB

data NothingEncoding
  = EmitNull
  | OmitKey

optionalNullable ::
  Fleece t =>
  NothingEncoding ->
  String ->
  (objectType -> Maybe a) ->
  Schema t a ->
  Field t objectType (Maybe a)
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

list :: Fleece t => Schema t a -> Schema t [a]
list itemSchema =
  transformAnonymous
    V.fromList
    V.toList
    (array itemSchema)

map :: (Fleece t, Typeable a) => Schema t a -> Schema t (Map.Map T.Text a)
map innerSchema =
  object $
    constructor id
      #* additionalFields id innerSchema

nonEmpty :: Fleece t => Schema t a -> Schema t (NEL.NonEmpty a)
nonEmpty itemSchema =
  let
    validateNonEmpty items =
      case NEL.nonEmpty items of
        Just nonEmptyItems -> Right nonEmptyItems
        Nothing -> Left "Expected non-empty array for NonEmpty list, but array was empty"
  in
    validateAnonymous
      NEL.toList
      validateNonEmpty
      (list itemSchema)

data SetDuplicateHandling
  = AllowInputDuplicates
  | RejectInputDuplicates

set :: (Ord a, Fleece t) => SetDuplicateHandling -> Schema t a -> Schema t (Set.Set a)
set handling itemSchema =
  case handling of
    AllowInputDuplicates ->
      transformAnonymous
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
        validateAnonymous
          (V.fromList . Set.toList)
          validateNoDuplicates
          (array itemSchema)

nonEmptyText :: Fleece t => Schema t NET.NonEmptyText
nonEmptyText =
  let
    validateNonEmptyText value =
      case NET.fromText value of
        Just net -> Right net
        Nothing -> Left "Expected non-empty text for NonEmptyText, but text was empty"
  in
    validateAnonymous
      NET.toText
      validateNonEmptyText
      text

integer :: Fleece t => Schema t Integer
integer =
  unboundedIntegralNumber

-- | Validates a 'number', failing if the precision is too high for the given resolution.
fixed ::
  (Fleece t, HasResolution r) =>
  Schema t (Fixed r)
fixed =
  validateAnonymous
    realToFrac
    fixedFromScientific
    number

fixedFromScientific :: forall r. HasResolution r => Scientific -> Either String (Fixed r)
fixedFromScientific sci =
  let
    normalized = normalize sci
    fixedResolution = resolution (Proxy :: Proxy r)
    sciExponent = base10Exponent normalized
  in
    if sciExponent >= 0 || 10 ^ abs sciExponent <= fixedResolution
      then Right (realToFrac normalized)
      else
        Left $
          "Invalid fixed precision number. Max precision is "
            <> show (floor (logBase 10 (fromInteger fixedResolution) :: Double) :: Integer)
            <> " decimal places."

string :: Fleece t => Schema t String
string = transformAnonymous T.pack T.unpack text

utcTime :: Fleece t => Schema t Time.UTCTime
utcTime =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date-time"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.utcTime
      }

utcTimeWithFormat :: Fleece t => String -> Schema t Time.UTCTime
utcTimeWithFormat = timeWithFormat "UTCTime"

localTime :: Fleece t => Schema t Time.LocalTime
localTime =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date-time-local"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.localTime
      }

localTimeWithFormat :: Fleece t => String -> Schema t Time.LocalTime
localTimeWithFormat = timeWithFormat "LocalTime"

zonedTime :: Fleece t => Schema t Time.ZonedTime
zonedTime =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date-time"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.zonedTime
      }

zonedTimeWithFormat :: Fleece t => String -> Schema t Time.ZonedTime
zonedTimeWithFormat = timeWithFormat "ZonedTime"

day :: Fleece t => Schema t Time.Day
day =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.day
      }

dayWithFormat :: Fleece t => String -> Schema t Time.Day
dayWithFormat = timeWithFormat "Day"

timeWithFormat :: (Time.FormatTime time, Time.ParseTime time) => Fleece t => String -> String -> Schema t time
timeWithFormat typeName formatString =
  let
    decode raw =
      case Time.parseTimeM False Time.defaultTimeLocale formatString raw of
        Just success -> Right success
        Nothing ->
          Left $
            "Invalid " <> typeName <> ", custom format is: " <> formatString
  in
    format formatString $
      validateNamed
        (unqualifiedName typeName)
        (Time.formatTime Time.defaultTimeLocale formatString)
        decode
        string

data ISO8601Format time
  = ISO8601Format
  { iso8601FormatLogical :: String
  , iso8601FormatString :: ISO8601.Format time
  , iso8601FormatParser :: AttoText.Parser time
  }

-- An internal helper for building building time schemes
iso8601Formatted ::
  Fleece t =>
  ISO8601Format time ->
  Schema t time
iso8601Formatted iso8601Format =
  let
    formatLogical =
      iso8601FormatLogical iso8601Format

    formatString =
      iso8601FormatString iso8601Format

    parser =
      iso8601FormatParser iso8601Format

    parseTime jsonText =
      case AttoText.parseOnly (parser <* AttoText.endOfInput) jsonText of
        Right time ->
          Right time
        Left err ->
          Left $
            "Invalid time format for "
              <> formatLogical
              <> ", value "
              <> show jsonText
              <> ": "
              <> err

    baseSchema =
      validateAnonymous
        (T.pack . ISO8601.formatShow formatString)
        parseTime
        text
  in
    format formatLogical baseSchema
