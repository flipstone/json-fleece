{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Aeson.Encoder
  ( Encoder (..)
  , encode
  , encodeStrict
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LEnc
import qualified Data.Vector as V
import GHC.TypeLits (KnownSymbol, symbolVal)
import Shrubbery (type (@=))
import qualified Shrubbery

import qualified Fleece.Core as FC

data Encoder a
  = Encoder FC.Name (a -> Aeson.Encoding)

encode :: Encoder a -> a -> LBS.ByteString
encode (Encoder _name toEncoding) =
  AesonEncoding.encodingToLazyByteString . toEncoding

encodeStrict :: Encoder a -> a -> BS.ByteString
encodeStrict (Encoder _name toEncoding) =
  LBS.toStrict . AesonEncoding.encodingToLazyByteString . toEncoding

instance FC.Fleece Encoder where
  newtype Object Encoder object _constructor
    = Object (object -> Aeson.Series)

  newtype Field Encoder object _a
    = Field (object -> Aeson.Series)

  newtype AdditionalFields Encoder object _a
    = AdditionalFields (object -> Aeson.Series)

  newtype UnionMembers Encoder _allTypes handledTypes
    = UnionMembers (Shrubbery.BranchBuilder handledTypes Aeson.Encoding)

  newtype TaggedUnionMembers Encoder _allTags handledTags
    = TaggedUnionMembers (Shrubbery.TaggedBranchBuilder handledTags (T.Text, Aeson.Series))

  type Validator Encoder = FC.StandardValidator

  schemaName (Encoder name _toEncoding) =
    name

  number =
    Encoder (FC.unqualifiedName "number") Aeson.toEncoding

  text =
    Encoder (FC.unqualifiedName "text") Aeson.toEncoding

  boolean =
    Encoder (FC.unqualifiedName "boolean") Aeson.toEncoding

  null =
    Encoder
      (FC.unqualifiedName "null")
      (\FC.Null -> Aeson.toEncoding Aeson.Null)

  array (Encoder name itemToEncoding) =
    Encoder
      (FC.annotateName name "array")
      (AesonTypes.listEncoding itemToEncoding . V.toList)

  nullable (Encoder name toEncoding) =
    Encoder (FC.annotateName name "nullable") $ \mbValue ->
      case mbValue of
        Left FC.Null -> Aeson.toEncoding Aeson.Null
        Right value -> toEncoding value

  required name accessor (Encoder _name toEncoding) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        AesonEncoding.pair key (toEncoding (accessor object))

  optional name accessor (Encoder _name toEncoding) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        case accessor object of
          Just value ->
            AesonEncoding.pair key (toEncoding value)
          Nothing ->
            mempty

  additionalFields accessor (Encoder _name toEncoding) =
    AdditionalFields $ \object ->
      Map.foldMapWithKey
        (\key value -> AesonEncoding.pair (AesonKey.fromText key) (toEncoding value))
        (accessor object)

  mapField _f encoder =
    coerce encoder

  constructor _f =
    Object (\_ -> mempty)

  field (Object mkStart) (Field mkNext) =
    Object $ \object ->
      mkStart object <> mkNext object

  additional (Object mkStart) (AdditionalFields mkRest) =
    Object $ \object ->
      mkStart object <> mkRest object

  objectNamed name (Object toSeries) =
    Encoder name (Aeson.pairs . toSeries)

  boundedEnumNamed name toText =
    Encoder name (Aeson.toEncoding . toText)

  validateNamed name validator (Encoder _unvalidatedName toEncoding) =
    Encoder name (toEncoding . FC.uncheck validator)

  unionNamed name (UnionMembers builder) =
    let
      branches =
        Shrubbery.branchBuild builder
    in
      Encoder name (Shrubbery.dissectUnion branches)

  unionMemberWithIndex _index encoder =
    UnionMembers $
      let
        -- It's important that this let is _inside_ the 'UnionMembers'
        -- constructor so that it lazy enough to allow the recursive reference
        -- of 'anyJSON' to itself within arrays.
        Encoder _name toEncoding = encoder
      in
        Shrubbery.singleBranch toEncoding

  unionCombine (UnionMembers left) (UnionMembers right) =
    UnionMembers (Shrubbery.appendBranches left right)

  taggedUnionNamed name tagProperty (TaggedUnionMembers builder) =
    let
      branches =
        Shrubbery.taggedBranchBuild builder

      tagPropText =
        T.pack tagProperty

      addTag (tagValue, aesonSeries) =
        Aeson.pairs $
          AesonEncoding.pair (AesonKey.fromText tagPropText) (Aeson.toEncoding tagValue)
            <> aesonSeries
    in
      Encoder name (addTag . Shrubbery.dissectTaggedUnion branches)

  taggedUnionMemberWithTag ::
    forall tag allTags a proxy.
    KnownSymbol tag =>
    proxy tag ->
    FC.Object Encoder a a ->
    FC.TaggedUnionMembers Encoder allTags '[tag @= a]
  taggedUnionMemberWithTag tag object =
    TaggedUnionMembers $
      let
        -- It's important that this let is _inside_ the 'UnionMembers'
        -- constructor so that it lazy enough to allow the recursive reference
        -- of 'anyJSON' to itself within arrays.
        Object toSeries = object

        tagValue =
          T.pack (symbolVal tag)
      in
        Shrubbery.taggedSingleBranch @tag (\a -> (tagValue, toSeries a))

  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (Shrubbery.appendTaggedBranches left right)

  jsonString (Encoder name toEncoding) =
    Encoder
      name
      ( Aeson.toEncoding
          . LEnc.decodeUtf8
          . AesonEncoding.encodingToLazyByteString
          . toEncoding
      )
