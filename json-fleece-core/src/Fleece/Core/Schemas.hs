{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- | Convenience schemas built from the primitives in "Fleece.Core.Class" for
common Haskell types.
-}
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
  , minItems
  , minLength
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

{- | Creates a union schema for @'Either' a b@ with a name derived from the
member schemas.
-}
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

-- | Like 'eitherOf', but with an explicit 'Name'.
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

{- | Creates an anonymous union schema with a name derived automatically from
the types.
-}
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

-- | Creates a union member schema, inferring the branch index automatically.
unionMember ::
  ( Fleece t
  , KnownNat branchIndex
  , branchIndex ~ FirstIndexOf a types
  ) =>
  Schema t a ->
  UnionMembers t types '[a]
unionMember =
  unionMemberWithIndex firstIndexOfType

{- | Creates a tagged union schema with a name derived automatically from the
tag types.
-}
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

{- | Creates a tagged union member schema, inferring the tag and index
automatically.
-}
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

{- | Creates a JSON object schema with a name derived automatically from the
type.
-}
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

{- | Creates a bounded enum schema with a name derived automatically from the
type.
-}
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

{- | Creates a validation schema with a name derived automatically from the
type.
-}
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

{- | Creates a schema for a type that is 'Coercible' to\/from another type's
schema. Name derived automatically.
-}
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

-- | Like 'coerceSchema', but with an explicit 'Name'.
coerceSchemaNamed ::
  (Fleece t, Coercible a b) =>
  Name ->
  Schema t b ->
  Schema t a
coerceSchemaNamed name schemaB =
  transformNamed name coerce coerce schemaB

-- | Like 'coerceSchema', but inherits the name of the base schema.
coerceSchemaAnonymous ::
  (Fleece t, Coercible a b) =>
  Schema t b ->
  Schema t a
coerceSchemaAnonymous schemaB =
  transformAnonymous coerce coerce schemaB

-- | Controls how 'Nothing' values are encoded for optional nullable fields.
data NothingEncoding
  = -- | Encode 'Nothing' as a JSON null value in the output.
    EmitNull
  | -- | Omit the key entirely from the JSON output when the value is 'Nothing'.
    OmitKey

{- | Creates a field that is both optional and nullable, collapsing the nested
'Maybe'\/'Either Null' into a simple 'Maybe'.
-}
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

{- | Creates a schema for a list by converting to\/from a 'V.Vector' array
schema.
-}
list :: Fleece t => Schema t a -> Schema t [a]
list itemSchema =
  transformAnonymous
    V.fromList
    V.toList
    (array itemSchema)

{- | Creates a schema for a @'Map.Map' 'T.Text' a@ as a JSON object with
additional fields.
-}
map :: (Fleece t, Typeable a) => Schema t a -> Schema t (Map.Map T.Text a)
map innerSchema =
  object $
    constructor id
      #* additionalFields id innerSchema

{- | Creates a schema for a 'NEL.NonEmpty' list, validating that the array is
non-empty.
-}
nonEmpty :: Fleece t => Schema t a -> Schema t (NEL.NonEmpty a)
nonEmpty itemSchema =
  let
    validateNonEmpty items =
      case NEL.nonEmpty items of
        Just nonEmptyItems -> Right nonEmptyItems
        Nothing -> Left "Expected non-empty array for NonEmpty list, but array was empty"
  in
    minItems 1 $
      validateAnonymous
        NEL.toList
        validateNonEmpty
        (list itemSchema)

{- | Controls how duplicate values are handled when decoding a set from a JSON
array.
-}
data SetDuplicateHandling
  = -- | Silently discard duplicate values when decoding.
    AllowInputDuplicates
  | -- | Fail decoding if duplicate values are found in the input.
    RejectInputDuplicates

{- | Creates a schema for a 'Set.Set' from an array, with configurable
duplicate handling.
-}
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

{- | Creates a schema for 'NET.NonEmptyText', validating that the text is
non-empty.
-}
nonEmptyText :: Fleece t => Schema t NET.NonEmptyText
nonEmptyText =
  let
    validateNonEmptyText value =
      case NET.fromText value of
        Just net -> Right net
        Nothing -> Left "Expected non-empty text for NonEmptyText, but text was empty"
  in
    minLength 1 $
      validateAnonymous
        NET.toText
        validateNonEmptyText
        text

-- | A schema for 'Integer' values, as unbounded integral JSON numbers.
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

-- | Creates a schema for 'String' values by converting to\/from 'T.Text'.
string :: Fleece t => Schema t String
string = transformAnonymous T.pack T.unpack text

-- | A schema for 'Time.UTCTime' in ISO 8601 date-time format.
utcTime :: Fleece t => Schema t Time.UTCTime
utcTime =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date-time"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.utcTime
      }

-- | A schema for 'Time.UTCTime' with a custom time format string.
utcTimeWithFormat :: Fleece t => String -> Schema t Time.UTCTime
utcTimeWithFormat = timeWithFormat "UTCTime"

-- | A schema for 'Time.LocalTime' in ISO 8601 local date-time format.
localTime :: Fleece t => Schema t Time.LocalTime
localTime =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date-time-local"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.localTime
      }

-- | A schema for 'Time.LocalTime' with a custom time format string.
localTimeWithFormat :: Fleece t => String -> Schema t Time.LocalTime
localTimeWithFormat = timeWithFormat "LocalTime"

-- | A schema for 'Time.ZonedTime' in ISO 8601 date-time format.
zonedTime :: Fleece t => Schema t Time.ZonedTime
zonedTime =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date-time"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.zonedTime
      }

-- | A schema for 'Time.ZonedTime' with a custom time format string.
zonedTimeWithFormat :: Fleece t => String -> Schema t Time.ZonedTime
zonedTimeWithFormat = timeWithFormat "ZonedTime"

-- | A schema for 'Time.Day' in ISO 8601 date format.
day :: Fleece t => Schema t Time.Day
day =
  iso8601Formatted $
    ISO8601Format
      { iso8601FormatLogical = "date"
      , iso8601FormatString = ISO8601.iso8601Format
      , iso8601FormatParser = AttoTime.day
      }

-- | A schema for 'Time.Day' with a custom time format string.
dayWithFormat :: Fleece t => String -> Schema t Time.Day
dayWithFormat = timeWithFormat "Day"

{- | Creates a time schema with a custom format string using
'Time.formatTime'\/'Time.parseTimeM'.
-}
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
