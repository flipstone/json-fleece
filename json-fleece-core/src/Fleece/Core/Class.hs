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
      , Validator
      , schemaName
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
      , boundedEnumNamed
      , unionNamed
      , unionMemberWithIndex
      , unionCombine
      , taggedUnionNamed
      , taggedUnionMemberWithTag
      , taggedUnionCombine
      , jsonString
      )
  , (#+)
  , (#*)
  , (#|)
  , (#@)
  , Null (Null)
  ) where

import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.TypeLits (KnownNat, KnownSymbol)
import Shrubbery (BranchIndex, Tag, TagIndex, TagType, TaggedTypes, TaggedUnion, TypeAtIndex, Union, type (@=))
import Shrubbery.TypeList (Append, Length)

import Fleece.Core.Name (Name)
import Fleece.Core.Validator (FleeceValidator)

class FleeceValidator (Validator schema) => Fleece schema where
  data Object schema :: Type -> Type -> Type
  data Field schema :: Type -> Type -> Type
  data AdditionalFields schema :: Type -> Type -> Type
  data UnionMembers schema :: [Type] -> [Type] -> Type
  data TaggedUnionMembers schema :: [Tag] -> [Tag] -> Type
  data Validator schema :: Type -> Type -> Type

  schemaName :: schema a -> Name

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
    constructor ->
    Object schema object constructor

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
    Validator schema a b ->
    schema a ->
    schema b

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
