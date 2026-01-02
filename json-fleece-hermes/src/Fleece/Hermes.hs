{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Hermes
  ( Decoder (Decoder, toDecoder)
  , decode
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Hermes as H
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import GHC.TypeLits (KnownNat, KnownSymbol, symbolVal)
import Shrubbery (type (@=))
import qualified Shrubbery

import qualified Fleece.Core as FC

newtype Decoder a
  = Decoder {toDecoder :: H.Decoder a}

decode :: FC.Schema Decoder a -> BS.ByteString -> Either String a
decode decoder input =
  first (T.unpack . H.formatException) $
    H.decodeEither (toDecoder (FC.schemaInterpreter decoder)) input

instance FC.Fleece Decoder where
  data Object Decoder _object a = Object
    { objectFields :: [Text]
    , objectDecoder :: H.FieldsDecoder a
    }

  data Field Decoder _object a = Field
    { fieldName :: Text
    , fieldDecoder :: H.FieldsDecoder a
    }

  newtype AdditionalFields Decoder _object a = AdditionalFields
    { additionalFieldsDecoder :: [Text] -> H.FieldsDecoder a
    }

  newtype UnionMembers Decoder allTypes _handledTypes
    = UnionMembers (FC.Name -> H.Decoder (Shrubbery.Union allTypes))

  newtype TaggedUnionMembers Decoder allTags _handledTags
    = TaggedUnionMembers (Map.Map T.Text (H.FieldsDecoder (Shrubbery.TaggedUnion allTags)))

  interpretFormat _ =
    FC.schemaInterpreter

  {-# INLINE interpretNumber #-}
  interpretNumber _name =
    Decoder H.scientific

  {-# INLINE interpretText #-}
  interpretText _name =
    Decoder H.text

  {-# INLINE interpretBoolean #-}
  interpretBoolean _name =
    Decoder H.bool

  {-# INLINE interpretArray #-}
  interpretArray _arrayName (FC.Schema _itemSchemaName (Decoder itemFromValue)) =
    Decoder $
      H.vector itemFromValue

  {-# INLINE interpretNull #-}
  interpretNull _name =
    Decoder $ do
      isNull <- H.isNull
      if isNull
        then pure FC.Null
        else fail "expected null"

  {-# INLINE interpretNullable #-}
  interpretNullable _nullableName (FC.Schema _schemaName (Decoder parseValue)) =
    Decoder $ do
      isNull <- H.isNull
      if isNull
        then pure (Left FC.Null)
        else fmap Right parseValue

  {-# INLINE required #-}
  required name _accessor (FC.Schema _name (Decoder parseValue)) =
    let
      key = T.pack name
    in
      Field key (H.atKey key parseValue)

  {-# INLINE optional #-}
  optional name _accessor (FC.Schema _name (Decoder parseValue)) =
    let
      key = T.pack name
    in
      Field key (H.atKeyOptional key parseValue)

  {-# INLINE additionalFields #-}
  additionalFields _accessor (FC.Schema _name (Decoder parseValue)) =
    AdditionalFields $ \definedFields ->
      H.liftObjectDecoder $ H.objectAsMapExcluding definedFields pure parseValue

  {-# INLINE mapField #-}
  mapField f (Field name parseField) =
    Field name (fmap f parseField)

  {-# INLINE constructor #-}
  constructor f =
    Object [] (pure f)

  {-# INLINE field #-}
  field (Object fieldNames parseF) field =
    Object
      { objectFields = fieldName field : fieldNames
      , objectDecoder =
          parseF <*> fieldDecoder field
      }

  {-# INLINE additional #-}
  additional (Object fieldNames parseF) fields =
    Object
      { objectFields = fieldNames
      , objectDecoder =
          parseF <*> additionalFieldsDecoder fields fieldNames
      }

  {-# INLINE interpretObjectNamed #-}
  interpretObjectNamed _name (Object _definedFields parseObject) =
    Decoder $ H.object parseObject

  {-# INLINE interpretBoundedEnumNamed #-}
  interpretBoundedEnumNamed name toText =
    let
      decodingMap =
        Map.fromList
          . map (\e -> (toText e, e))
          $ [minBound .. maxBound]
    in
      Decoder $
        H.withText $ \textValue ->
          case Map.lookup textValue decodingMap of
            Just enumValue -> pure enumValue
            Nothing ->
              fail $
                "Unrecognized value for "
                  <> FC.nameToString name
                  <> " enum: "
                  <> show textValue

  interpretValidateNamed name _uncheck check (FC.Schema _unvalidatedName (Decoder parseValue)) =
    validatingDecoder name parseValue check

  interpretValidateAnonymous _uncheck check (FC.Schema unvalidatedName (Decoder parseValue)) =
    validatingDecoder unvalidatedName parseValue check

  {-# INLINE interpretUnionNamed #-}
  interpretUnionNamed name (UnionMembers parseMembers) =
    Decoder (parseMembers name)

  {-# INLINE unionMemberWithIndex #-}
  unionMemberWithIndex index (FC.Schema _name (Decoder parseMember)) =
    UnionMembers (\_name -> fmap (Shrubbery.unifyUnion index) parseMember)

  {-# INLINE unionCombine #-}
  unionCombine (UnionMembers parseLeft) (UnionMembers parseRight) =
    UnionMembers $ \name ->
      parseLeft name
        <|> parseRight name
        <|> fail ("All union parsing options for " <> FC.nameUnqualified name <> " failed.")

  {-# INLINE interpretTaggedUnionNamed #-}
  interpretTaggedUnionNamed name tagProperty (TaggedUnionMembers parserMap) =
    let
      tagPropKey =
        T.pack tagProperty

      nameString =
        FC.nameToString name
    in
      Decoder $
        H.object $ do
          tagValue <- H.atKey tagPropKey H.text
          case Map.lookup tagValue parserMap of
            Just parseFields -> parseFields
            Nothing -> fail ("Invalid tag found for tagged union " <> nameString <> ": " <> T.unpack tagValue)

  taggedUnionMemberWithTag ::
    forall tag allTags a proxy n.
    ( KnownSymbol tag
    , n ~ Shrubbery.TagIndex tag allTags
    , KnownNat n
    , Shrubbery.TagType tag allTags ~ a
    , Shrubbery.TypeAtIndex n (Shrubbery.TaggedTypes allTags) ~ a
    ) =>
    proxy tag ->
    FC.Object Decoder a a ->
    FC.TaggedUnionMembers Decoder allTags '[tag @= a]
  taggedUnionMemberWithTag tag object =
    let
      tagValue =
        T.pack (symbolVal tag)

      parseUnion =
        fmap (Shrubbery.unifyTaggedUnion @tag)
          . objectDecoder
          $ object
    in
      TaggedUnionMembers (Map.singleton tagValue parseUnion)

  {-# INLINE taggedUnionCombine #-}
  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (Map.union left right)

  interpretJsonString (FC.Schema _name (Decoder parseValue)) =
    Decoder $
      H.withText $ \jsonText -> do
        -- hermes currently cannot decode scalars. So we make a dummy object.
        let
          obj = Enc.encodeUtf8 $ T.pack "{ \"\": " <> jsonText <> T.pack "}"
        case H.decodeEither (H.object $ H.atKey (T.pack "") parseValue) obj of
          Left err -> fail ("Error decoding nested json string: " <> show err)
          Right value -> pure value

validatingDecoder ::
  FC.Name ->
  H.Decoder a ->
  (a -> Either String b) ->
  Decoder b
validatingDecoder name parseValue check =
  Decoder $ do
    uncheckedValue <- parseValue
    case check uncheckedValue of
      Right checkedValue -> pure checkedValue
      Left err -> fail $ "Error validating " <> FC.nameToString name <> ": " <> err
