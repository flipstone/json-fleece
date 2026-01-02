{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Aeson.ToValue
  ( ToValue (ToValue)
  , toValue
  , toLazyText
  , toStrictText
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as TL
import GHC.TypeLits (KnownSymbol, symbolVal)
import Shrubbery (type (@=))
import qualified Shrubbery

import qualified Fleece.Core as FC

toLazyText :: FC.Schema ToValue a -> a -> TL.Text
toLazyText encoder = encodeToLazyText . toValue encoder

toStrictText :: FC.Schema ToValue a -> a -> T.Text
toStrictText encoder = TL.toStrict . toLazyText encoder

toValue :: FC.Schema ToValue a -> a -> Aeson.Value
toValue (FC.Schema _name (ToValue f)) = f

-- This needs to be a separate `Fleece` instance because using `Value` for
-- encoding results in a different encoded result than using `Encoding`. The
-- insertion order is not preserved, so while `encode schema a` is effectively
-- the same and will be decoded the same as `toJSON (toValue a)`, it will not
-- be exactly the same bytestring produced by the two.
--
newtype ToValue a = ToValue (a -> Aeson.Value)

instance FC.Fleece ToValue where
  newtype Object ToValue object _constructor
    = Object (object -> [AesonTypes.Pair])

  newtype Field ToValue object _a
    = Field (object -> [AesonTypes.Pair])

  newtype AdditionalFields ToValue object _a
    = AdditionalFields (object -> [AesonTypes.Pair])

  newtype UnionMembers ToValue _allTypes handledTypes
    = UnionMembers (Shrubbery.BranchBuilder handledTypes Aeson.Value)

  newtype TaggedUnionMembers ToValue _allTypes handledTypes
    = TaggedUnionMembers (Shrubbery.TaggedBranchBuilder handledTypes (T.Text, [AesonTypes.Pair]))

  interpretFormat _ =
    FC.schemaInterpreter

  interpretNumber _name =
    ToValue Aeson.toJSON

  interpretText _name =
    ToValue Aeson.toJSON

  interpretBoolean _name =
    ToValue Aeson.toJSON

  interpretNull _name =
    ToValue (\FC.Null -> Aeson.Null)

  interpretArray _arrayName (FC.Schema _itemSchemaName (ToValue itemToJSON)) =
    ToValue (Aeson.Array . fmap itemToJSON)

  interpretNullable _nullableName (FC.Schema _schemaName (ToValue toJSON)) =
    ToValue $ \mbValue ->
      case mbValue of
        Left FC.Null -> Aeson.Null
        Right value -> toJSON value

  required name accessor (FC.Schema _name (ToValue toJSON)) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        [(key, toJSON (accessor object))]

  optional name accessor (FC.Schema _name (ToValue toJSON)) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        case accessor object of
          Just value ->
            [(key, toJSON value)]
          Nothing ->
            []

  additionalFields accessor (FC.Schema _name (ToValue toJSON)) =
    AdditionalFields $ \object ->
      map (\(key, value) -> (AesonKey.fromText key, toJSON value))
        . Map.toList
        . accessor
        $ object

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

  interpretObjectNamed _name (Object toObject) =
    ToValue (Aeson.object . toObject)

  interpretBoundedEnumNamed _name toText =
    ToValue (Aeson.toJSON . toText)

  interpretValidateNamed _name uncheck _check (FC.Schema _unvalidatedName (ToValue toJSON)) =
    ToValue (toJSON . uncheck)

  interpretValidateAnonymous uncheck _check (FC.Schema _unvalidatedName (ToValue toJSON)) =
    ToValue (toJSON . uncheck)

  interpretUnionNamed _name (UnionMembers builder) =
    let
      branches =
        Shrubbery.branchBuild builder
    in
      ToValue (Shrubbery.dissectUnion branches)

  unionMemberWithIndex _index (FC.Schema _name encoder) =
    UnionMembers $
      let
        -- It's important that this let is _inside_ the 'UnionMembers'
        -- constructor so that it lazy enough to allow the recursive reference
        -- of 'anyJSON' to itself within arrays.
        ToValue toJSON = encoder
      in
        Shrubbery.singleBranch toJSON

  unionCombine (UnionMembers left) (UnionMembers right) =
    UnionMembers (Shrubbery.appendBranches left right)

  interpretTaggedUnionNamed _name tagProperty (TaggedUnionMembers builder) =
    let
      branches =
        Shrubbery.taggedBranchBuild builder

      tagKey =
        AesonKey.fromString tagProperty
    in
      ToValue $ \value ->
        let
          (tagValue, fields) =
            Shrubbery.dissectTaggedUnion branches value
        in
          Aeson.object ((tagKey, Aeson.toJSON tagValue) : fields)

  taggedUnionMemberWithTag ::
    forall tag allTags a proxy.
    KnownSymbol tag =>
    proxy tag ->
    FC.Object ToValue a a ->
    FC.TaggedUnionMembers ToValue allTags '[tag @= a]
  taggedUnionMemberWithTag tagV (Object mkFields) =
    let
      tagValue =
        T.pack (symbolVal tagV)
    in
      TaggedUnionMembers (Shrubbery.taggedSingleBranch @tag (\a -> (tagValue, mkFields a)))

  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (Shrubbery.appendTaggedBranches left right)

  interpretJsonString (FC.Schema _name (ToValue toJSON)) =
    ToValue
      ( Aeson.String
          . Enc.decodeUtf8
          . LBS.toStrict
          . Aeson.encode
          . toJSON
      )
