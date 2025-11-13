{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Aeson.ToValue
  ( ToValue
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

toLazyText :: ToValue a -> a -> TL.Text
toLazyText encoder = encodeToLazyText . toValue encoder

toStrictText :: ToValue a -> a -> T.Text
toStrictText encoder = TL.toStrict . toLazyText encoder

toValue :: ToValue a -> a -> Aeson.Value
toValue (ToValue _name f) =
  f

-- This needs to be a separate `Fleece` instance because using `Value` for
-- encoding results in a different encoded result than using `Encoding`. The
-- insertion order is not preserved, so while `encode schema a` is effectively
-- the same and will be decoded the same as `toJSON (toValue a)`, it will not
-- be exactly the same bytestring produced by the two.
--
data ToValue a
  = ToValue FC.Name (a -> Aeson.Value)

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

  schemaName (ToValue name _toJSON) =
    name

  format _ =
    id

  number =
    ToValue (FC.unqualifiedName "number") Aeson.toJSON

  text =
    ToValue (FC.unqualifiedName "text") Aeson.toJSON

  boolean =
    ToValue (FC.unqualifiedName "boolean") Aeson.toJSON

  null =
    ToValue
      (FC.unqualifiedName "null")
      (\FC.Null -> Aeson.Null)

  array (ToValue name itemToJSON) =
    ToValue
      (FC.annotateName name "array")
      (Aeson.Array . fmap itemToJSON)

  nullable (ToValue name toJSON) =
    ToValue (FC.annotateName name "nullable") $ \mbValue ->
      case mbValue of
        Left FC.Null -> Aeson.Null
        Right value -> toJSON value

  required name accessor (ToValue _name toJSON) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        [(key, toJSON (accessor object))]

  optional name accessor (ToValue _name toJSON) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        case accessor object of
          Just value ->
            [(key, toJSON value)]
          Nothing ->
            []

  additionalFields accessor (ToValue _name toJSON) =
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

  objectNamed name (Object toObject) =
    ToValue name (Aeson.object . toObject)

  boundedEnumNamed name toText =
    ToValue name (Aeson.toJSON . toText)

  validateNamed name uncheck _check (ToValue _unvalidatedName toJSON) =
    ToValue name (toJSON . uncheck)

  validateAnonymous uncheck _check (ToValue unvalidatedName toJSON) =
    ToValue unvalidatedName (toJSON . uncheck)

  unionNamed name (UnionMembers builder) =
    let
      branches =
        Shrubbery.branchBuild builder
    in
      ToValue name (Shrubbery.dissectUnion branches)

  unionMemberWithIndex _index encoder =
    UnionMembers $
      let
        -- It's important that this let is _inside_ the 'UnionMembers'
        -- constructor so that it lazy enough to allow the recursive reference
        -- of 'anyJSON' to itself within arrays.
        ToValue _name toJSON = encoder
      in
        Shrubbery.singleBranch toJSON

  unionCombine (UnionMembers left) (UnionMembers right) =
    UnionMembers (Shrubbery.appendBranches left right)

  taggedUnionNamed name tagProperty (TaggedUnionMembers builder) =
    let
      branches =
        Shrubbery.taggedBranchBuild builder

      tagKey =
        AesonKey.fromString tagProperty
    in
      ToValue name $ \value ->
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

  jsonString (ToValue name toJSON) =
    ToValue
      name
      ( Aeson.String
          . Enc.decodeUtf8
          . LBS.toStrict
          . Aeson.encode
          . toJSON
      )
