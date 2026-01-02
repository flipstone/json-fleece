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
  = Encoder (a -> Aeson.Encoding)

encode :: FC.Schema Encoder a -> a -> LBS.ByteString
encode (FC.Schema _name (Encoder toEncoding)) =
  AesonEncoding.encodingToLazyByteString . toEncoding

encodeStrict :: FC.Schema Encoder a -> a -> BS.ByteString
encodeStrict (FC.Schema _name (Encoder toEncoding)) =
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

  interpretFormat _ =
    FC.schemaInterpreter

  interpretNumber _name =
    Encoder Aeson.toEncoding

  interpretText _name =
    Encoder Aeson.toEncoding

  interpretBoolean _name =
    Encoder Aeson.toEncoding

  interpretNull _name =
    Encoder (\FC.Null -> Aeson.toEncoding Aeson.Null)

  interpretArray _arrayName (FC.Schema _itemSchemaName (Encoder itemToEncoding)) =
    Encoder (AesonTypes.listEncoding itemToEncoding . V.toList)

  interpretNullable _nullableName (FC.Schema _schemaName (Encoder toEncoding)) =
    Encoder $ \mbValue ->
      case mbValue of
        Left FC.Null -> Aeson.toEncoding Aeson.Null
        Right value -> toEncoding value

  required name accessor (FC.Schema _schemaName (Encoder toEncoding)) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        AesonEncoding.pair key (toEncoding (accessor object))

  optional name accessor (FC.Schema _schemaName (Encoder toEncoding)) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        case accessor object of
          Just value ->
            AesonEncoding.pair key (toEncoding value)
          Nothing ->
            mempty

  additionalFields accessor (FC.Schema _schemaName (Encoder toEncoding)) =
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

  interpretObjectNamed _name (Object toSeries) =
    Encoder (Aeson.pairs . toSeries)

  interpretBoundedEnumNamed _name toText =
    Encoder (Aeson.toEncoding . toText)

  interpretValidateNamed _name uncheck _check (FC.Schema _unvalidatedName (Encoder toEncoding)) =
    Encoder (toEncoding . uncheck)

  interpretValidateAnonymous uncheck _check (FC.Schema _unvalidatedName (Encoder toEncoding)) =
    Encoder (toEncoding . uncheck)

  interpretUnionNamed _name (UnionMembers builder) =
    let
      branches =
        Shrubbery.branchBuild builder
    in
      Encoder (Shrubbery.dissectUnion branches)

  unionMemberWithIndex _index (FC.Schema _schemaName encoder) =
    UnionMembers $
      let
        -- It's important that this let is _inside_ the 'UnionMembers'
        -- constructor so that it lazy enough to allow the recursive reference
        -- of 'anyJSON' to itself within arrays.
        Encoder toEncoding = encoder
      in
        Shrubbery.singleBranch toEncoding

  unionCombine (UnionMembers left) (UnionMembers right) =
    UnionMembers (Shrubbery.appendBranches left right)

  interpretTaggedUnionNamed _name tagProperty (TaggedUnionMembers builder) =
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
      Encoder (addTag . Shrubbery.dissectTaggedUnion branches)

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

  interpretJsonString (FC.Schema _name (Encoder toEncoding)) =
    Encoder
      ( Aeson.toEncoding
          . LEnc.decodeUtf8
          . AesonEncoding.encodingToLazyByteString
          . toEncoding
      )
