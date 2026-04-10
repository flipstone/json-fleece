{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- | Core types and the 'Fleece' typeclass for building JSON schema
interpreters. The 'Fleece' class defines a set of primitives and combinators
that can be implemented to provide different interpretations of JSON schemas
(e.g., encoding, decoding, documentation generation).
-}
module Fleece.Core.Class
  ( Fleece
      ( Field
      , AdditionalFields
      , Object
      , UnionMembers
      , TaggedUnionMembers
      , interpretDescribe
      , interpretFormat
      , interpretText
      , interpretNumber
      , interpretBoolean
      , interpretArray
      , interpretNull
      , required
      , optional
      , mapField
      , additionalFields
      , interpretObjectNamed
      , constructor
      , interpretNullable
      , field
      , additional
      , interpretValidateNamed
      , interpretValidateAnonymous
      , interpretBoundedEnumNamed
      , interpretUnionNamed
      , unionMemberWithIndex
      , unionCombine
      , interpretTaggedUnionNamed
      , taggedUnionMemberWithTag
      , taggedUnionCombine
      , interpretJsonString
      , interpretMinLength
      , interpretMaxLength
      , interpretMinItems
      , interpretMaxItems
      , interpretMinimum
      , interpretMaximum
      , -- \* Helpers with default implementations
        interpretInt
      , interpretInt8
      , interpretInt16
      , interpretInt32
      , interpretInt64
      , interpretWord
      , interpretWord8
      , interpretWord16
      , interpretWord32
      , interpretWord64
      , interpretDouble
      , interpretFloat
      )
  , Schema (..)
  , hoistSchema
  , (#+)
  , (#*)
  , (#|)
  , (#@)
  , Null (Null)
  , transform
  , transformNamed
  , transformAnonymous
  , boundedIntegralNumber
  , boundedIntegralNumberNamed
  , boundedIntegralNumberAnonymous
  , unboundedIntegralNumber
  , unboundedIntegralNumberNamed
  , unboundedIntegralNumberAnonymous
  , realFloat
  , realFloatNamed
  , realFloatAnonymous
  , describe
  , format
  , text
  , number
  , boolean
  , array
  , null
  , objectNamed
  , nullable
  , validateNamed
  , validateAnonymous
  , boundedEnumNamed
  , unionNamed
  , taggedUnionNamed
  , jsonString
  , minLength
  , maxLength
  , minItems
  , maxItems
  , Fleece.Core.Class.minimum
  , Fleece.Core.Class.maximum
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
  ) where

import qualified Data.Int as I
import Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.NonEmptyText as NET
import Data.Scientific (Scientific, floatingOrInteger, fromFloatDigits, toBoundedInteger, toRealFloat)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Word as W
import GHC.TypeLits (KnownNat, KnownSymbol)
import Shrubbery (BranchIndex, Tag, TagIndex, TagType, TaggedTypes, TaggedUnion, TypeAtIndex, Union, type (@=))
import Shrubbery.TypeList (Append, Length)
import Prelude hiding (maximum, minimum, null)

import Fleece.Core.Name (Name, annotateName, defaultSchemaName, nameToString, unqualifiedName)

-- | A named JSON schema that pairs a 'Name' with an interpreter of type @t a@.
data Schema t a = Schema
  { schemaName :: Name
  -- ^ The 'Name' associated with this schema.
  , schemaInterpreter :: t a
  {- ^ The interpreter value that implements this schema for a particular
  Fleece instance.
  -}
  }

-- | Transforms the interpreter within a schema using a natural transformation.
hoistSchema :: (forall x. s x -> t x) -> Schema s a -> Schema t a
hoistSchema f schema =
  schema
    { schemaInterpreter = f (schemaInterpreter schema)
    }

{- | The core typeclass for defining JSON schema interpreters. Instances of
this class provide an interpretation for each JSON schema primitive and
combinator. For example, an Aeson decoder instance interprets schemas as
parsers, while a pretty printer interprets them as formatting functions.
-}
class Fleece t where
  {- | Represents a JSON object schema under construction, parameterized by
  the object type and the constructor type being built.
  -}
  data Object t :: Type -> Type -> Type

  -- | Represents a single field within a JSON object schema.
  data Field t :: Type -> Type -> Type

  {- | Represents a catch-all for additional key\/value pairs in a JSON object
  beyond the defined fields.
  -}
  data AdditionalFields t :: Type -> Type -> Type

  {- | Represents the set of member schemas being assembled for an anonymous
  union.
  -}
  data UnionMembers t :: [Type] -> [Type] -> Type

  {- | Represents the set of member schemas being assembled for a tagged union,
  where each member is identified by a tag value.
  -}
  data TaggedUnionMembers t :: [Tag] -> [Tag] -> Type

  {- | Hook for interpreters to handle schema descriptions. Default
  implementation ignores the description.
  -}
  interpretDescribe :: NET.NonEmptyText -> Schema t a -> t a
  interpretDescribe _net = schemaInterpreter

  -- | Hook for interpreters to handle format annotations on a schema.
  interpretFormat :: String -> Schema t a -> t a

  -- | Hook for interpreters to handle the JSON number primitive.
  interpretNumber :: Name -> t Scientific

  -- | Hook for interpreters to handle the JSON text\/string primitive.
  interpretText :: Name -> t T.Text

  -- | Hook for interpreters to handle the JSON boolean primitive.
  interpretBoolean :: Name -> t Bool

  -- | Hook for interpreters to handle JSON array schemas.
  interpretArray :: Name -> Schema t a -> t (V.Vector a)

  -- | Hook for interpreters to handle the JSON null value.
  interpretNull :: Name -> t Null

  {- | Hook for interpreters to handle nullable schemas, which decode to
  @'Either' 'Null' a@.
  -}
  interpretNullable :: Name -> Schema t a -> t (Either Null a)

  -- | Defines a required field in a JSON object schema.
  required ::
    String ->
    (object -> a) ->
    Schema t a ->
    Field t object a

  {- | Defines an optional field in a JSON object schema. The field may be
  absent from the JSON object.
  -}
  optional ::
    String ->
    (object -> Maybe a) ->
    Schema t a ->
    Field t object (Maybe a)

  -- | Maps a function over the result type of a field.
  mapField ::
    (a -> b) ->
    Field t object a ->
    Field t object b

  {- | Captures all key\/value pairs in a JSON object that are not covered by
  defined fields.
  -}
  additionalFields ::
    (object -> Map.Map T.Text a) ->
    Schema t a ->
    AdditionalFields t object (Map.Map T.Text a)

  -- | Hook for interpreters to finalize a JSON object schema with a given name.
  interpretObjectNamed ::
    Name ->
    Object t a a ->
    t a

  {- | Begins constructing a JSON object schema by providing the constructor
  function.
  -}
  constructor ::
    constructorType ->
    Object t object constructorType

  -- | Adds a field to a JSON object schema under construction.
  field ::
    Object t object (a -> b) ->
    Field t object a ->
    Object t object b

  {- | Adds additional (catch-all) fields to a JSON object schema under
  construction.
  -}
  additional ::
    Object t object (a -> object) ->
    AdditionalFields t object a ->
    Object t object object

  -- | Hook for interpreters to handle named validation schemas.
  interpretValidateNamed ::
    Name ->
    (a -> b) ->
    (b -> Either String a) ->
    Schema t b ->
    t a

  -- | Hook for interpreters to handle anonymous validation schemas.
  interpretValidateAnonymous ::
    (a -> b) ->
    (b -> Either String a) ->
    Schema t b ->
    t a

  -- | Hook for interpreters to handle bounded enum schemas.
  interpretBoundedEnumNamed ::
    (Bounded a, Enum a) =>
    Name ->
    (a -> T.Text) ->
    t a

  -- | Hook for interpreters to finalize an anonymous union schema.
  interpretUnionNamed ::
    KnownNat (Length types) =>
    Name ->
    UnionMembers t types types ->
    t (Union types)

  {- | Hook for interpreters to create a single union member schema at a
  specific type index.
  -}
  unionMemberWithIndex ::
    BranchIndex a types ->
    Schema t a ->
    UnionMembers t types '[a]

  -- | Hook for interpreters to combine two sets of union member schemas.
  unionCombine ::
    UnionMembers t types left ->
    UnionMembers t types right ->
    UnionMembers t types (Append left right)

  -- | Hook for interpreters to finalize a tagged union schema.
  interpretTaggedUnionNamed ::
    KnownNat (Length (TaggedTypes tags)) =>
    Name ->
    String ->
    TaggedUnionMembers t tags tags ->
    t (TaggedUnion tags)

  {- | Hook for interpreters to create a tagged union member with a specific
  tag.
  -}
  taggedUnionMemberWithTag ::
    ( KnownSymbol tag
    , n ~ TagIndex tag tags
    , KnownNat n
    , TagType tag tags ~ a
    , TypeAtIndex n (TaggedTypes tags) ~ a
    ) =>
    proxy tag ->
    Object t a a ->
    TaggedUnionMembers t tags '[tag @= a]

  -- | Hook for interpreters to combine two sets of tagged union member schemas.
  taggedUnionCombine ::
    Append (TaggedTypes left) (TaggedTypes right)
      ~ TaggedTypes (Append left right) =>
    TaggedUnionMembers t tags left ->
    TaggedUnionMembers t tags right ->
    TaggedUnionMembers t tags (Append left right)

  {- | Hook for interpreters to handle schemas for values encoded as JSON
  strings within JSON.
  -}
  interpretJsonString ::
    Schema t a ->
    t a

  {- | Hook for interpreters to handle minimum text length annotations. Default
  implementation ignores the annotation.
  -}
  interpretMinLength :: Integer -> Schema t a -> t a
  interpretMinLength _ = schemaInterpreter

  {- | Hook for interpreters to handle maximum text length annotations. Default
  implementation ignores the annotation.
  -}
  interpretMaxLength :: Integer -> Schema t a -> t a
  interpretMaxLength _ = schemaInterpreter

  {- | Hook for interpreters to handle minimum item count annotations. Default
  implementation ignores the annotation.
  -}
  interpretMinItems :: Integer -> Schema t a -> t a
  interpretMinItems _ = schemaInterpreter

  {- | Hook for interpreters to handle maximum item count annotations. Default
  implementation ignores the annotation.
  -}
  interpretMaxItems :: Integer -> Schema t a -> t a
  interpretMaxItems _ = schemaInterpreter

  {- | Hook for interpreters to handle minimum numeric value annotations.
  Default implementation ignores the annotation.
  -}
  interpretMinimum :: Integer -> Schema t a -> t a
  interpretMinimum _ = schemaInterpreter

  {- | Hook for interpreters to handle maximum numeric value annotations.
  Default implementation ignores the annotation.
  -}
  interpretMaximum :: Integer -> Schema t a -> t a
  interpretMaximum _ = schemaInterpreter

  {- | Hook for interpreters to handle 'Int' values. Can be overridden for
  optimized implementations.
  -}
  interpretInt :: Name -> t Int
  interpretInt _ = schemaInterpreter boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'I.Int8' values. Can be overridden for
  optimized implementations.
  -}
  interpretInt8 :: Name -> t I.Int8
  interpretInt8 _ = schemaInterpreter $ format "int8" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'I.Int16' values. Can be overridden for
  optimized implementations.
  -}
  interpretInt16 :: Name -> t I.Int16
  interpretInt16 _ = schemaInterpreter $ format "int16" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'I.Int32' values. Can be overridden for
  optimized implementations.
  -}
  interpretInt32 :: Name -> t I.Int32
  interpretInt32 _ = schemaInterpreter $ format "int32" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'I.Int64' values. Can be overridden for
  optimized implementations.
  -}
  interpretInt64 :: Name -> t I.Int64
  interpretInt64 _ = schemaInterpreter $ format "int64" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'Word' values. Can be overridden for
  optimized implementations.
  -}
  interpretWord :: Name -> t Word
  interpretWord _ = schemaInterpreter $ format "word" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'W.Word8' values. Can be overridden for
  optimized implementations.
  -}
  interpretWord8 :: Name -> t W.Word8
  interpretWord8 _ = schemaInterpreter $ format "word8" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'W.Word16' values. Can be overridden for
  optimized implementations.
  -}
  interpretWord16 :: Name -> t W.Word16
  interpretWord16 _ = schemaInterpreter $ format "word16" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'W.Word32' values. Can be overridden for
  optimized implementations.
  -}
  interpretWord32 :: Name -> t W.Word32
  interpretWord32 _ = schemaInterpreter $ format "word32" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'W.Word64' values. Can be overridden for
  optimized implementations.
  -}
  interpretWord64 :: Name -> t W.Word64
  interpretWord64 _ = schemaInterpreter $ format "word64" boundedIntegralNumberAnonymous

  {- | Hook for interpreters to handle 'Double' values. Can be overridden for
  optimized implementations.
  -}
  interpretDouble :: Name -> t Double
  interpretDouble _ = schemaInterpreter $ format "double" realFloatAnonymous

  {- | Hook for interpreters to handle 'Float' values. Can be overridden for
  optimized implementations.
  -}
  interpretFloat :: Name -> t Float
  interpretFloat _ = schemaInterpreter $ format "float" realFloatAnonymous

{- | Attaches a text description to a schema. The description may be used by
documentation-generating interpreters.
-}
describe :: Fleece t => T.Text -> Schema t a -> Schema t a
describe desc schema =
  schema
    { schemaInterpreter =
        case NET.fromText desc of
          Just net -> interpretDescribe net schema
          Nothing -> schemaInterpreter schema
    }

-- | Attaches a format annotation to a schema (e.g., @"date-time"@, @"int32"@).
format :: Fleece t => String -> Schema t a -> Schema t a
format fmt schema =
  schema
    { schemaInterpreter = interpretFormat fmt schema
    }

numberName :: Name
numberName =
  unqualifiedName "number"

-- | A schema for JSON numbers, represented as 'Scientific'.
number :: Fleece t => Schema t Scientific
number =
  Schema
    { schemaName = numberName
    , schemaInterpreter = interpretNumber numberName
    }

textName :: Name
textName =
  unqualifiedName "text"

-- | A schema for JSON text strings, represented as 'T.Text'.
text :: Fleece t => Schema t T.Text
text =
  Schema
    { schemaName = textName
    , schemaInterpreter = interpretText textName
    }

booleanName :: Name
booleanName =
  unqualifiedName "boolean"

-- | A schema for JSON booleans.
boolean :: Fleece t => Schema t Bool
boolean =
  Schema
    { schemaName = booleanName
    , schemaInterpreter = interpretBoolean booleanName
    }

nullName :: Name
nullName =
  unqualifiedName "null"

-- | A schema for JSON null values, represented as the 'Null' type.
null :: Fleece t => Schema t Null
null =
  Schema
    { schemaName = nullName
    , schemaInterpreter = interpretNull nullName
    }

{- | A schema for JSON arrays with a given item schema, represented as a
'V.Vector'.
-}
array :: Fleece t => Schema t a -> Schema t (V.Vector a)
array schema =
  let
    arrayName =
      annotateName (schemaName schema) "array"
  in
    Schema
      { schemaName = arrayName
      , schemaInterpreter = interpretArray arrayName schema
      }

-- | Wraps a schema to allow null values, resulting in @'Either' 'Null' a@.
nullable :: Fleece t => Schema t a -> Schema t (Either Null a)
nullable schema =
  let
    nullableName =
      annotateName (schemaName schema) "nullable"
  in
    Schema
      { schemaName = nullableName
      , schemaInterpreter = interpretNullable nullableName schema
      }

-- | Creates a named JSON object schema from an 'Object' definition.
objectNamed ::
  Fleece t =>
  Name ->
  Object t a a ->
  Schema t a
objectNamed name object =
  Schema
    { schemaName = name
    , schemaInterpreter = interpretObjectNamed name object
    }

{- | Creates a named schema that validates values of one type as values of
another.
-}
validateNamed ::
  Fleece t =>
  Name ->
  (a -> b) ->
  (b -> Either String a) ->
  Schema t b ->
  Schema t a
validateNamed name check uncheck schema =
  Schema
    { schemaName = name
    , schemaInterpreter = interpretValidateNamed name check uncheck schema
    }

-- | Like 'validateNamed', but inherits the name of the base schema.
validateAnonymous ::
  Fleece t =>
  (a -> b) ->
  (b -> Either String a) ->
  Schema t b ->
  Schema t a
validateAnonymous check uncheck schema =
  Schema
    { schemaName = schemaName schema
    , schemaInterpreter = interpretValidateAnonymous check uncheck schema
    }

{- | Creates a named schema for a bounded enum type, using a function to
convert each value to text.
-}
boundedEnumNamed ::
  (Fleece t, Bounded a, Enum a) =>
  Name ->
  (a -> T.Text) ->
  Schema t a
boundedEnumNamed name toText =
  Schema
    { schemaName = name
    , schemaInterpreter = interpretBoundedEnumNamed name toText
    }

-- | Creates a named schema for an anonymous union of types.
unionNamed ::
  (Fleece t, KnownNat (Length types)) =>
  Name ->
  UnionMembers t types types ->
  Schema t (Union types)
unionNamed name members =
  Schema
    { schemaName = name
    , schemaInterpreter = interpretUnionNamed name members
    }

{- | Creates a named schema for a tagged union, where the tag is a JSON object
field.
-}
taggedUnionNamed ::
  ( Fleece t
  , KnownNat (Length (TaggedTypes tags))
  ) =>
  Name ->
  String ->
  TaggedUnionMembers t tags tags ->
  Schema t (TaggedUnion tags)
taggedUnionNamed name tagField members =
  Schema
    { schemaName = name
    , schemaInterpreter = interpretTaggedUnionNamed name tagField members
    }

{- | Wraps a schema to handle values that are JSON-encoded within a JSON
string.
-}
jsonString ::
  Fleece t =>
  Schema t a ->
  Schema t a
jsonString schema =
  Schema
    { schemaName = schemaName schema
    , schemaInterpreter = interpretJsonString schema
    }

{- | Declares that the schema already validates a minimum text length. This
does not perform any validation itself — it informs Fleece of validation you
are already performing so that it can be reflected in schema descriptions.
-}
minLength :: Fleece t => Integer -> Schema t a -> Schema t a
minLength len schema =
  schema
    { schemaInterpreter = interpretMinLength len schema
    }

{- | Declares that the schema already validates a maximum text length. This
does not perform any validation itself — it informs Fleece of validation you
are already performing so that it can be reflected in schema descriptions.
-}
maxLength :: Fleece t => Integer -> Schema t a -> Schema t a
maxLength len schema =
  schema
    { schemaInterpreter = interpretMaxLength len schema
    }

{- | Declares that the schema already validates a minimum number of items. This
does not perform any validation itself — it informs Fleece of validation you
are already performing so that it can be reflected in schema descriptions.
-}
minItems :: Fleece t => Integer -> Schema t a -> Schema t a
minItems len schema =
  schema
    { schemaInterpreter = interpretMinItems len schema
    }

{- | Declares that the schema already validates a maximum number of items. This
does not perform any validation itself — it informs Fleece of validation you
are already performing so that it can be reflected in schema descriptions.
-}
maxItems :: Fleece t => Integer -> Schema t a -> Schema t a
maxItems len schema =
  schema
    { schemaInterpreter = interpretMaxItems len schema
    }

{- | Declares that the schema already validates a minimum numeric value. This
does not perform any validation itself — it informs Fleece of validation you
are already performing so that it can be reflected in schema descriptions.
-}
minimum :: Fleece t => Integer -> Schema t a -> Schema t a
minimum len schema =
  schema
    { schemaInterpreter = interpretMinimum len schema
    }

{- | Declares that the schema already validates a maximum numeric value. This
does not perform any validation itself — it informs Fleece of validation you
are already performing so that it can be reflected in schema descriptions.
-}
maximum :: Fleece t => Integer -> Schema t a -> Schema t a
maximum len schema =
  schema
    { schemaInterpreter = interpretMaximum len schema
    }

intName :: Name
intName =
  unqualifiedName "int"

-- | A schema for 'Int' values.
int :: Fleece t => Schema t Int
int =
  Schema
    { schemaName = intName
    , schemaInterpreter = interpretInt intName
    }

int8Name :: Name
int8Name =
  unqualifiedName "int8"

-- | A schema for 'I.Int8' values.
int8 :: Fleece t => Schema t I.Int8
int8 =
  Schema
    { schemaName = int8Name
    , schemaInterpreter = interpretInt8 int8Name
    }

int16Name :: Name
int16Name =
  unqualifiedName "int16"

-- | A schema for 'I.Int16' values.
int16 :: Fleece t => Schema t I.Int16
int16 =
  Schema
    { schemaName = int16Name
    , schemaInterpreter = interpretInt16 int16Name
    }

int32Name :: Name
int32Name =
  unqualifiedName "int32"

-- | A schema for 'I.Int32' values.
int32 :: Fleece t => Schema t I.Int32
int32 =
  Schema
    { schemaName = int32Name
    , schemaInterpreter = interpretInt32 int32Name
    }

int64Name :: Name
int64Name =
  unqualifiedName "int64"

-- | A schema for 'I.Int64' values.
int64 :: Fleece t => Schema t I.Int64
int64 =
  Schema
    { schemaName = int64Name
    , schemaInterpreter = interpretInt64 int64Name
    }

wordName :: Name
wordName =
  unqualifiedName "word"

-- | A schema for 'Word' values.
word :: Fleece t => Schema t Word
word =
  Schema
    { schemaName = wordName
    , schemaInterpreter = interpretWord wordName
    }

word8Name :: Name
word8Name =
  unqualifiedName "word8"

-- | A schema for 'W.Word8' values.
word8 :: Fleece t => Schema t W.Word8
word8 =
  Schema
    { schemaName = word8Name
    , schemaInterpreter = interpretWord8 word8Name
    }

word16Name :: Name
word16Name =
  unqualifiedName "word16"

-- | A schema for 'W.Word16' values.
word16 :: Fleece t => Schema t W.Word16
word16 =
  Schema
    { schemaName = word16Name
    , schemaInterpreter = interpretWord16 word16Name
    }

word32Name :: Name
word32Name =
  unqualifiedName "word32"

-- | A schema for 'W.Word32' values.
word32 :: Fleece t => Schema t W.Word32
word32 =
  Schema
    { schemaName = word32Name
    , schemaInterpreter = interpretWord32 word32Name
    }

word64Name :: Name
word64Name =
  unqualifiedName "word64"

-- | A schema for 'W.Word64' values.
word64 :: Fleece t => Schema t W.Word64
word64 =
  Schema
    { schemaName = word64Name
    , schemaInterpreter = interpretWord64 word64Name
    }

doubleName :: Name
doubleName =
  unqualifiedName "double"

-- | A schema for 'Double' values.
double :: Fleece t => Schema t Double
double =
  Schema
    { schemaName = doubleName
    , schemaInterpreter = interpretDouble doubleName
    }

floatName :: Name
floatName =
  unqualifiedName "float"

-- | A schema for 'Float' values.
float :: Fleece t => Schema t Float
float =
  Schema
    { schemaName = floatName
    , schemaInterpreter = interpretFloat floatName
    }

instance Fleece t => Functor (Field t object) where
  fmap = mapField

-- | Operator for adding a field to an object schema. Equivalent to 'field'.
(#+) ::
  Fleece t =>
  Object t object (a -> b) ->
  Field t object a ->
  Object t object b
(#+) =
  field

infixl 9 #+

{- | Operator for adding additional fields to an object schema. Equivalent to
'additional'.
-}
(#*) ::
  Fleece t =>
  Object t object (a -> object) ->
  AdditionalFields t object a ->
  Object t object object
(#*) =
  additional

infixl 9 #*

-- | Operator for combining union member schemas. Equivalent to 'unionCombine'.
(#|) ::
  Fleece t =>
  UnionMembers t types left ->
  UnionMembers t types right ->
  UnionMembers t types (Append left right)
(#|) =
  unionCombine

{- | Operator for combining tagged union member schemas. Equivalent to
'taggedUnionCombine'.
-}
(#@) ::
  ( Fleece t
  , Append (TaggedTypes left) (TaggedTypes right) ~ TaggedTypes (Append left right)
  ) =>
  TaggedUnionMembers t types left ->
  TaggedUnionMembers t types right ->
  TaggedUnionMembers t types (Append left right)
(#@) =
  taggedUnionCombine

infixl 9 #|

-- | A type representing JSON null.
data Null
  = Null
  deriving (Eq, Show)

---
-- BEGIN helpers that need to be in this module for default implementations to use
-- (or for symmetry with other helpers that are used in default implementations)
---

{- | Creates a schema for a new type by providing total conversion functions to
and from an existing schema's type. The name is derived automatically.
-}
transform ::
  (Fleece t, Typeable a) =>
  (a -> b) ->
  (b -> a) ->
  Schema t b ->
  Schema t a
transform aToB bToA schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      transformNamed name aToB bToA schemaB
  in
    schemaA

-- | Like 'transform', but with an explicitly provided 'Name'.
transformNamed ::
  Fleece t =>
  Name ->
  (a -> b) ->
  (b -> a) ->
  Schema t b ->
  Schema t a
transformNamed name aToB bToA =
  validateNamed name aToB (Right . bToA)

-- | Like 'transform', but inherits the name from the underlying schema.
transformAnonymous ::
  Fleece t =>
  (a -> b) ->
  (b -> a) ->
  Schema t b ->
  Schema t a
transformAnonymous aToB bToA =
  validateAnonymous aToB (Right . bToA)

{- | Creates a named schema for an unbounded integral number type, validating
that JSON numbers are integral.
-}
unboundedIntegralNumberNamed ::
  (Fleece t, Integral n) =>
  Name ->
  Schema t n
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

{- | Like 'unboundedIntegralNumberNamed', but inherits the name from the number
schema.
-}
unboundedIntegralNumberAnonymous ::
  (Fleece t, Integral n) =>
  Schema t n
unboundedIntegralNumberAnonymous =
  let
    asDouble :: Double -> a -> a
    asDouble _ = id

    validateInteger s =
      case floatingOrInteger s of
        Right n -> pure n
        Left f ->
          asDouble f $
            Left $
              "Error parsing bounded integer value. Value not integral: "
                <> show s
  in
    validateAnonymous
      fromIntegral
      validateInteger
      number

-- | Like 'unboundedIntegralNumberNamed', but derives the name automatically.
unboundedIntegralNumber ::
  (Fleece t, Integral n, Typeable n) =>
  Schema t n
unboundedIntegralNumber =
  let
    name =
      defaultSchemaName schema

    schema =
      unboundedIntegralNumberNamed name
  in
    schema

{- | Creates a named schema for a bounded integral number type, validating that
JSON numbers are integral and within bounds.
-}
boundedIntegralNumberNamed ::
  (Fleece t, Integral n, Bounded n) =>
  Name ->
  Schema t n
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

{- | Like 'boundedIntegralNumberNamed', but inherits the name from the number
schema.
-}
boundedIntegralNumberAnonymous ::
  (Fleece t, Integral n, Bounded n) =>
  Schema t n
boundedIntegralNumberAnonymous =
  let
    validateInteger s =
      case toBoundedInteger s of
        Just n -> pure n
        Nothing ->
          Left $
            "Error parsing bounded integer value for. Value not integral, or exceeds the bounds of the expected type: "
              <> show s
  in
    validateAnonymous
      fromIntegral
      validateInteger
      number

-- | Like 'boundedIntegralNumberNamed', but derives the name automatically.
boundedIntegralNumber ::
  (Fleece t, Integral n, Bounded n, Typeable n) =>
  Schema t n
boundedIntegralNumber =
  let
    name =
      defaultSchemaName schema

    schema =
      boundedIntegralNumberNamed name
  in
    schema

{- | Creates a schema for a 'RealFloat' type by converting to\/from
'Scientific'. Name is derived automatically.
-}
realFloat ::
  (Fleece t, RealFloat f, Typeable f) =>
  Schema t f
realFloat =
  let
    name =
      defaultSchemaName schema

    schema =
      realFloatNamed name
  in
    schema

-- | Like 'realFloat', but with an explicitly provided 'Name'.
realFloatNamed ::
  (Fleece t, RealFloat f) =>
  Name ->
  Schema t f
realFloatNamed name =
  transformNamed
    name
    fromFloatDigits
    toRealFloat
    number

-- | Like 'realFloat', but inherits the name from the number schema.
realFloatAnonymous ::
  (Fleece t, RealFloat f) =>
  Schema t f
realFloatAnonymous =
  transformAnonymous
    fromFloatDigits
    toRealFloat
    number
