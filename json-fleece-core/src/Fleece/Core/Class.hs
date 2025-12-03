{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Core.Class
  ( Fleece
      ( Field
      , AdditionalFields
      , Object
      , UnionMembers
      , TaggedUnionMembers
      , schemaName
      , format
      , text
      , number
      , boolean
      , array
      , null
      , required
      , optional
      , mapField
      , additionalFields
      , objectNamed
      , constructor
      , nullable
      , field
      , additional
      , validateNamed
      , validateAnonymous
      , boundedEnumNamed
      , unionNamed
      , unionMemberWithIndex
      , unionCombine
      , taggedUnionNamed
      , taggedUnionMemberWithTag
      , taggedUnionCombine
      , jsonString
      , -- \* Helpers with default implementations
        int
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
      )
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

import Fleece.Core.Name (Name, defaultSchemaName, nameToString)

class Fleece schema where
  data Object schema :: Type -> Type -> Type
  data Field schema :: Type -> Type -> Type
  data AdditionalFields schema :: Type -> Type -> Type
  data UnionMembers schema :: [Type] -> [Type] -> Type
  data TaggedUnionMembers schema :: [Tag] -> [Tag] -> Type

  schemaName :: schema a -> Name

  format :: String -> schema a -> schema a

  number :: schema Scientific

  text :: schema T.Text

  boolean :: schema Bool

  array :: schema a -> schema (V.Vector a)

  null :: schema Null

  nullable :: schema a -> schema (Either Null a)

  required ::
    String ->
    (object -> a) ->
    schema a ->
    Field schema object a

  optional ::
    String ->
    (object -> Maybe a) ->
    schema a ->
    Field schema object (Maybe a)

  mapField ::
    (a -> b) ->
    Field schema object a ->
    Field schema object b

  additionalFields ::
    (object -> Map.Map T.Text a) ->
    schema a ->
    AdditionalFields schema object (Map.Map T.Text a)

  objectNamed ::
    Name ->
    Object schema a a ->
    schema a

  constructor ::
    constructorType ->
    Object schema object constructorType

  field ::
    Object schema object (a -> b) ->
    Field schema object a ->
    Object schema object b

  additional ::
    Object schema object (a -> object) ->
    AdditionalFields schema object a ->
    Object schema object object

  validateNamed ::
    Name ->
    (a -> b) ->
    (b -> Either String a) ->
    (schema b) ->
    (schema a)

  validateAnonymous ::
    (a -> b) ->
    (b -> Either String a) ->
    (schema b) ->
    (schema a)

  boundedEnumNamed ::
    (Bounded a, Enum a) =>
    Name ->
    (a -> T.Text) ->
    schema a

  unionNamed ::
    KnownNat (Length types) =>
    Name ->
    UnionMembers schema types types ->
    schema (Union types)

  unionMemberWithIndex ::
    BranchIndex a types ->
    schema a ->
    UnionMembers schema types '[a]

  unionCombine ::
    UnionMembers schema types left ->
    UnionMembers schema types right ->
    UnionMembers schema types (Append left right)

  taggedUnionNamed ::
    KnownNat (Length (TaggedTypes tags)) =>
    Name ->
    String ->
    TaggedUnionMembers schema tags tags ->
    schema (TaggedUnion tags)

  taggedUnionMemberWithTag ::
    ( KnownSymbol tag
    , n ~ TagIndex tag tags
    , KnownNat n
    , TagType tag tags ~ a
    , TypeAtIndex n (TaggedTypes tags) ~ a
    ) =>
    proxy tag ->
    Object schema a a ->
    TaggedUnionMembers schema tags '[tag @= a]

  taggedUnionCombine ::
    Append (TaggedTypes left) (TaggedTypes right)
      ~ TaggedTypes (Append left right) =>
    TaggedUnionMembers schema tags left ->
    TaggedUnionMembers schema tags right ->
    TaggedUnionMembers schema tags (Append left right)

  jsonString ::
    schema a ->
    schema a

  -- Members that have default implementations, but can been overridden by
  -- specific implementations
  int :: schema Int
  int = boundedIntegralNumberAnonymous

  int8 :: schema I.Int8
  int8 = format "int8" boundedIntegralNumberAnonymous

  int16 :: schema I.Int16
  int16 = format "int16" boundedIntegralNumberAnonymous

  int32 :: schema I.Int32
  int32 = format "int32" boundedIntegralNumberAnonymous

  int64 :: schema I.Int64
  int64 = format "int64" boundedIntegralNumberAnonymous

  word :: schema Word
  word = format "word" boundedIntegralNumberAnonymous

  word8 :: schema W.Word8
  word8 = format "word8" boundedIntegralNumberAnonymous

  word16 :: schema W.Word16
  word16 = format "word16" boundedIntegralNumberAnonymous

  word32 :: schema W.Word32
  word32 = format "word32" boundedIntegralNumberAnonymous

  word64 :: schema W.Word64
  word64 = format "word64" boundedIntegralNumberAnonymous

  double :: schema Double
  double = format "double" realFloatAnonymous

  float :: schema Float
  float = format "float" realFloatAnonymous

instance Fleece schema => Functor (Field schema object) where
  fmap = mapField

(#+) ::
  Fleece schema =>
  Object schema object (a -> b) ->
  Field schema object a ->
  Object schema object b
(#+) =
  field

infixl 9 #+

(#*) ::
  Fleece schema =>
  Object schema object (a -> object) ->
  AdditionalFields schema object a ->
  Object schema object object
(#*) =
  additional

infixl 9 #*

(#|) ::
  Fleece schema =>
  UnionMembers schema types left ->
  UnionMembers schema types right ->
  UnionMembers schema types (Append left right)
(#|) =
  unionCombine

(#@) ::
  ( Fleece schema
  , Append (TaggedTypes left) (TaggedTypes right) ~ TaggedTypes (Append left right)
  ) =>
  TaggedUnionMembers schema types left ->
  TaggedUnionMembers schema types right ->
  TaggedUnionMembers schema types (Append left right)
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

transformAnonymous ::
  Fleece schema =>
  (a -> b) ->
  (b -> a) ->
  schema b ->
  schema a
transformAnonymous aToB bToA =
  validateAnonymous aToB (Right . bToA)

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

unboundedIntegralNumberAnonymous ::
  (Fleece schema, Integral n) =>
  schema n
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

boundedIntegralNumberAnonymous ::
  (Fleece schema, Integral n, Bounded n) =>
  schema n
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

realFloatAnonymous ::
  (Fleece schema, RealFloat f) =>
  schema f
realFloatAnonymous =
  transformAnonymous
    fromFloatDigits
    toRealFloat
    number
