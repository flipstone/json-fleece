{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Fleece.Core.Class
  ( Fleece
      ( Field
      , AdditionalFields
      , Object
      , UnionMembers
      , TaggedUnionMembers
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
import Data.Scientific (Scientific, floatingOrInteger, fromFloatDigits, toBoundedInteger, toRealFloat)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Word as W
import GHC.TypeLits (KnownNat, KnownSymbol)
import Shrubbery (BranchIndex, Tag, TagIndex, TagType, TaggedTypes, TaggedUnion, TypeAtIndex, Union, type (@=))
import Shrubbery.TypeList (Append, Length)
import Prelude hiding (null)

import Fleece.Core.Name (Name, annotateName, defaultSchemaName, nameToString, unqualifiedName)

data Schema t a = Schema
  { schemaName :: Name
  , schemaInterpreter :: t a
  }

hoistSchema :: (forall x. s x -> t x) -> Schema s a -> Schema t a
hoistSchema f schema =
  schema
    { schemaInterpreter = f (schemaInterpreter schema)
    }

class Fleece t where
  data Object t :: Type -> Type -> Type
  data Field t :: Type -> Type -> Type
  data AdditionalFields t :: Type -> Type -> Type
  data UnionMembers t :: [Type] -> [Type] -> Type
  data TaggedUnionMembers t :: [Tag] -> [Tag] -> Type

  interpretFormat :: String -> Schema t a -> t a

  interpretNumber :: Name -> t Scientific

  interpretText :: Name -> t T.Text

  interpretBoolean :: Name -> t Bool

  interpretArray :: Name -> Schema t a -> t (V.Vector a)

  interpretNull :: Name -> t Null

  interpretNullable :: Name -> Schema t a -> t (Either Null a)

  required ::
    String ->
    (object -> a) ->
    Schema t a ->
    Field t object a

  optional ::
    String ->
    (object -> Maybe a) ->
    Schema t a ->
    Field t object (Maybe a)

  mapField ::
    (a -> b) ->
    Field t object a ->
    Field t object b

  additionalFields ::
    (object -> Map.Map T.Text a) ->
    Schema t a ->
    AdditionalFields t object (Map.Map T.Text a)

  interpretObjectNamed ::
    Name ->
    Object t a a ->
    t a

  constructor ::
    constructorType ->
    Object t object constructorType

  field ::
    Object t object (a -> b) ->
    Field t object a ->
    Object t object b

  additional ::
    Object t object (a -> object) ->
    AdditionalFields t object a ->
    Object t object object

  interpretValidateNamed ::
    Name ->
    (a -> b) ->
    (b -> Either String a) ->
    Schema t b ->
    t a

  interpretValidateAnonymous ::
    (a -> b) ->
    (b -> Either String a) ->
    Schema t b ->
    t a

  interpretBoundedEnumNamed ::
    (Bounded a, Enum a) =>
    Name ->
    (a -> T.Text) ->
    t a

  interpretUnionNamed ::
    KnownNat (Length types) =>
    Name ->
    UnionMembers t types types ->
    t (Union types)

  unionMemberWithIndex ::
    BranchIndex a types ->
    Schema t a ->
    UnionMembers t types '[a]

  unionCombine ::
    UnionMembers t types left ->
    UnionMembers t types right ->
    UnionMembers t types (Append left right)

  interpretTaggedUnionNamed ::
    KnownNat (Length (TaggedTypes tags)) =>
    Name ->
    String ->
    TaggedUnionMembers t tags tags ->
    t (TaggedUnion tags)

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

  taggedUnionCombine ::
    Append (TaggedTypes left) (TaggedTypes right)
      ~ TaggedTypes (Append left right) =>
    TaggedUnionMembers t tags left ->
    TaggedUnionMembers t tags right ->
    TaggedUnionMembers t tags (Append left right)

  interpretJsonString ::
    Schema t a ->
    t a

  -- Members that have default implementations, but can been overridden by
  -- specific implementations
  interpretInt :: Name -> t Int
  interpretInt _ = schemaInterpreter boundedIntegralNumberAnonymous

  interpretInt8 :: Name -> t I.Int8
  interpretInt8 _ = schemaInterpreter $ format "int8" boundedIntegralNumberAnonymous

  interpretInt16 :: Name -> t I.Int16
  interpretInt16 _ = schemaInterpreter $ format "int16" boundedIntegralNumberAnonymous

  interpretInt32 :: Name -> t I.Int32
  interpretInt32 _ = schemaInterpreter $ format "int32" boundedIntegralNumberAnonymous

  interpretInt64 :: Name -> t I.Int64
  interpretInt64 _ = schemaInterpreter $ format "int64" boundedIntegralNumberAnonymous

  interpretWord :: Name -> t Word
  interpretWord _ = schemaInterpreter $ format "word" boundedIntegralNumberAnonymous

  interpretWord8 :: Name -> t W.Word8
  interpretWord8 _ = schemaInterpreter $ format "word8" boundedIntegralNumberAnonymous

  interpretWord16 :: Name -> t W.Word16
  interpretWord16 _ = schemaInterpreter $ format "word16" boundedIntegralNumberAnonymous

  interpretWord32 :: Name -> t W.Word32
  interpretWord32 _ = schemaInterpreter $ format "word32" boundedIntegralNumberAnonymous

  interpretWord64 :: Name -> t W.Word64
  interpretWord64 _ = schemaInterpreter $ format "word64" boundedIntegralNumberAnonymous

  interpretDouble :: Name -> t Double
  interpretDouble _ = schemaInterpreter $ format "double" realFloatAnonymous

  interpretFloat :: Name -> t Float
  interpretFloat _ = schemaInterpreter $ format "float" realFloatAnonymous

format :: Fleece t => String -> Schema t a -> Schema t a
format fmt schema =
  schema
    { schemaInterpreter = interpretFormat fmt schema
    }

numberName :: Name
numberName =
  unqualifiedName "number"

number :: Fleece t => Schema t Scientific
number =
  Schema
    { schemaName = numberName
    , schemaInterpreter = interpretNumber numberName
    }

textName :: Name
textName =
  unqualifiedName "text"

text :: Fleece t => Schema t T.Text
text =
  Schema
    { schemaName = textName
    , schemaInterpreter = interpretText textName
    }

booleanName :: Name
booleanName =
  unqualifiedName "boolean"

boolean :: Fleece t => Schema t Bool
boolean =
  Schema
    { schemaName = booleanName
    , schemaInterpreter = interpretBoolean booleanName
    }

nullName :: Name
nullName =
  unqualifiedName "null"

null :: Fleece t => Schema t Null
null =
  Schema
    { schemaName = nullName
    , schemaInterpreter = interpretNull nullName
    }

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
    , schemaInterpreter =
        interpretValidateNamed name check uncheck schema
    }

validateAnonymous ::
  Fleece t =>
  (a -> b) ->
  (b -> Either String a) ->
  Schema t b ->
  Schema t a
validateAnonymous check uncheck schema =
  Schema
    { schemaName = schemaName schema
    , schemaInterpreter =
        interpretValidateAnonymous check uncheck schema
    }

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
    , schemaInterpreter =
        interpretTaggedUnionNamed name tagField members
    }

jsonString ::
  Fleece t =>
  Schema t a ->
  Schema t a
jsonString schema =
  Schema
    { schemaName = schemaName schema
    , schemaInterpreter = interpretJsonString schema
    }

intName :: Name
intName =
  unqualifiedName "int"

int :: Fleece t => Schema t Int
int =
  Schema
    { schemaName = intName
    , schemaInterpreter = interpretInt intName
    }

int8Name :: Name
int8Name =
  unqualifiedName "int8"

int8 :: Fleece t => Schema t I.Int8
int8 =
  Schema
    { schemaName = int8Name
    , schemaInterpreter = interpretInt8 int8Name
    }

int16Name :: Name
int16Name =
  unqualifiedName "int16"

int16 :: Fleece t => Schema t I.Int16
int16 =
  Schema
    { schemaName = int16Name
    , schemaInterpreter = interpretInt16 int16Name
    }

int32Name :: Name
int32Name =
  unqualifiedName "int32"

int32 :: Fleece t => Schema t I.Int32
int32 =
  Schema
    { schemaName = int32Name
    , schemaInterpreter = interpretInt32 int32Name
    }

int64Name :: Name
int64Name =
  unqualifiedName "int64"

int64 :: Fleece t => Schema t I.Int64
int64 =
  Schema
    { schemaName = int64Name
    , schemaInterpreter = interpretInt64 int64Name
    }

wordName :: Name
wordName =
  unqualifiedName "word"

word :: Fleece t => Schema t Word
word =
  Schema
    { schemaName = wordName
    , schemaInterpreter = interpretWord wordName
    }

word8Name :: Name
word8Name =
  unqualifiedName "word8"

word8 :: Fleece t => Schema t W.Word8
word8 =
  Schema
    { schemaName = word8Name
    , schemaInterpreter = interpretWord8 word8Name
    }

word16Name :: Name
word16Name =
  unqualifiedName "word16"

word16 :: Fleece t => Schema t W.Word16
word16 =
  Schema
    { schemaName = word16Name
    , schemaInterpreter = interpretWord16 word16Name
    }

word32Name :: Name
word32Name =
  unqualifiedName "word32"

word32 :: Fleece t => Schema t W.Word32
word32 =
  Schema
    { schemaName = word32Name
    , schemaInterpreter = interpretWord32 word32Name
    }

word64Name :: Name
word64Name =
  unqualifiedName "word64"

word64 :: Fleece t => Schema t W.Word64
word64 =
  Schema
    { schemaName = word64Name
    , schemaInterpreter = interpretWord64 word64Name
    }

doubleName :: Name
doubleName =
  unqualifiedName "double"

double :: Fleece t => Schema t Double
double =
  Schema
    { schemaName = doubleName
    , schemaInterpreter = interpretDouble doubleName
    }

floatName :: Name
floatName =
  unqualifiedName "float"

float :: Fleece t => Schema t Float
float =
  Schema
    { schemaName = floatName
    , schemaInterpreter = interpretFloat floatName
    }

instance Fleece t => Functor (Field t object) where
  fmap = mapField

(#+) ::
  Fleece t =>
  Object t object (a -> b) ->
  Field t object a ->
  Object t object b
(#+) =
  field

infixl 9 #+

(#*) ::
  Fleece t =>
  Object t object (a -> object) ->
  AdditionalFields t object a ->
  Object t object object
(#*) =
  additional

infixl 9 #*

(#|) ::
  Fleece t =>
  UnionMembers t types left ->
  UnionMembers t types right ->
  UnionMembers t types (Append left right)
(#|) =
  unionCombine

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

data Null
  = Null
  deriving (Eq, Show)

---
-- BEGIN helpers that need to be in this module for default implementations to use
-- (or for symmetry with other helpers that are used in default implementations)
---

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

transformNamed ::
  Fleece t =>
  Name ->
  (a -> b) ->
  (b -> a) ->
  Schema t b ->
  Schema t a
transformNamed name aToB bToA =
  validateNamed name aToB (Right . bToA)

transformAnonymous ::
  Fleece t =>
  (a -> b) ->
  (b -> a) ->
  Schema t b ->
  Schema t a
transformAnonymous aToB bToA =
  validateAnonymous aToB (Right . bToA)

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

realFloatAnonymous ::
  (Fleece t, RealFloat f) =>
  Schema t f
realFloatAnonymous =
  transformAnonymous
    fromFloatDigits
    toRealFloat
    number
